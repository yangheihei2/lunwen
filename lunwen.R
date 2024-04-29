{
  {rm (list = ls ())
    options(scipen=999)
    options(digits = 20)
    library(readxl)
    library(openxlsx)
    library(dplyr)
    library(ggplot2)
    library(readr)
    library(sqldf)
    library(RSQLite)
    start_time <- Sys.time()
    order_20200201 <- read_csv("C:/Users/LAZC/Desktop/order_20200201.txt")
    distance_20200201 <- read_csv("C:/Users/LAZC/Desktop/distance_20200201.txt")
    courier_20200201 <- read_csv("C:/Users/LAZC/Desktop/courier_20200201.txt")
    courier_20200201<-courier_20200201[1:30,]
    action_20200201 <- read_excel("action_20200201.xlsx")
    order_20200201_clear_shop <- read_excel("order_20200201.xlsx")
    order_20200201_clear_shop<-order_20200201_clear_shop[1:300,]
  }
  #骑手初始位置
  courier_start_position<-sqldf("select distinct courier_id,courier_wave_start_lng,courier_wave_start_lat from action_20200201 ")
  
  #订单名单
  pb <- txtProgressBar(style=3)
  #初始化商铺坐标
  
  ##随机选定店铺数量，保证每个店铺至少有一个包裹
  #每个店铺有0.1的概率选择多拿包裹
  #初始化population
  population<-function(num){
    pickup_shop_package<-function(shop_id_test,gailv,max_load){
      #
      #
      i=1
      shop_total<-c()
      dingdan_total<-c()
      
      while(i<=max_load){
        
        #k1是第i家店铺id 根据店铺id找到该店铺所有订单id
        order_20200201_fake%>%select(shop_id,tracking_id)%>%filter(.,shop_id==shop_id_test[i])->k1sa
        
        
        if(length(k1sa$tracking_id)==1){
          shop_total[i]<-k1sa$shop_id
          dingdan_total[i]<-k1sa$tracking_id
          i<-i+1
        }else{
          a1<-runif(1, min = 0, max = 1)
          if(gailv>a1){
            
            a<-round((length(k1sa$tracking_id)-2)*runif(1,min=0,max=1)+2)
            pick_package_id<-sample(k1sa$tracking_id,a,replace = FALSE)
            shop_total[i:i+a-1]<-k1sa$shop_id
            dingdan_total[i:i+a-1]<-pick_package_id
            i<-i+a
          }else{
            pick_package_id<-sample(k1sa$tracking_id,1,replace = FALSE)
            shop_total[i]<-k1sa$shop_id
            dingdan_total[i]<-k1sa$tracking_id
            i<-i+1
          }}
        
        
        
      }
      togo_id<-data.frame(shop_id=shop_total,tracking_id=dingdan_total)
      return(togo_id)
      
    }
    order_20200201_fake<-order_20200201
    courier_20200201_fake<-courier_20200201
    population<-list()
    for(t in 1:num){
      #选定骑手
      
      acourier_id<-sample(courier_20200201_fake$courier_id, 1,replace=FALSE)
      courier_20200201%>%select(max_load,courier_id,speed)%>%filter(.,courier_id==acourier_id)->qishou
      max_num_load<-qishou$max_load
      speed<-qishou$speed
      #随机选定店铺数量，保证每个店铺至少有一个包裹
      #每个店铺有0.6的概率选择多拿包裹
      shop_id<-sample(order_20200201_clear_shop$shop_id,max_num_load,replace = FALSE) 
      package_id<-pickup_shop_package(shop_id,0.6,max_num_load)
      
      #show(package_id)
      #show(package_id_omit)#去NA
      #根据package_id_omit找shop_id
      popula<-list(acourier=acourier_id,max=max_num_load,speed=speed,shop_package=package_id)
      #拿到包裹后将该包裹id从总库中去除
      #####
      yanglujia<-order_20200201_fake$tracking_id%in%package_id$dingdan
      order_20200201_fake<-order_20200201_fake[!yanglujia, ]  
      yanglujia2<-courier_20200201_fake$courier_id%in%acourier_id
      courier_20200201_fake<-courier_20200201_fake[!yanglujia2, ]
      population[[t]] <- popula
      #选定订单数量
      #每个商家有概率选择特定数量的包裹
    }
    return(population)
  }
}

{
  let_distance<-function(a1,a2,a3,a4){
    return(((a1-a3)^2+(a2-a4)^2)^0.5)
  }
  #路径
  ######找到每个店铺对应包裹编号
  pick_shop<-function(b){
    #### b
    kong2<-b$shop_package  
    kong1<-b$shop_package$tracking_id
    heng_package<-numeric()
    zong_package<-numeric()
    
    for(i in 1:length(kong2$tracking_id)){
      order_20200201%>%select(deliver_lng,deliver_lat,tracking_id)%>%filter(.,tracking_id==kong1[i])->nuer
      zong_package[i]<-nuer$deliver_lng
      heng_package[i]<-nuer$deliver_lat
      
    }
    
    ####
    kong2$num<-c(1:length(kong2$tracking_id))
    kong3<-unique(kong2$shop_id)
    pick_shop<-list()
    for(u in 1:length(kong3)){
      kong4<-numeric()
      kong2%>%select(shop_id,num)%>%filter(.,shop_id==kong3[u])->kong4
      kong5<-kong4$num
      ###
      order_20200201_clear_shop%>%select(pick_lng,pick_lat,shop_id)%>%filter(.,shop_id==kong3[u])->shop_dizhi
      zong1<-unique(shop_dizhi$pick_lng)
      heng1<-unique(shop_dizhi$pick_lat)
      kong6<-list(shop_id=kong3[u],num=kong5,heng=heng1,zong=zong1)
      
      ###
      pick_shop[[u]]<-kong6
    }
    return(pick_shop)
  }
  ##把所有店铺和包裹统计一起方便path_true进行计算
  path_fake<-function(b){
    kong2<-b$shop_package  
    kong1<-b$shop_package$tracking_id
    kong2$num<-c(1:length(kong2$tracking_id))
    kong3<-unique(kong2$shop_id)
    ###
    dianpu<-c(1:length(kong3))
    baoguo<-c(1:length(kong2$tracking_id))
    courier_start_position%>%select(courier_id,courier_wave_start_lat,courier_wave_start_lng)%>%filter( . ,courier_id==b$acourier)->qishou2
    #######
    #计算当前骑手位置和店铺位置的距离 1是包裹 2是店铺
    distance<-data.frame(distance=rep(Inf,length(kong3)+length(kong2$tracking_id)),shop_or_package=c(rep(1, length(kong2$tracking_id)), rep(0, length(kong3))))
    c1<-as.character(kong2$tracking_id)
    distance$num<-c(c1,kong3)
    m<-sqldf("select DISTINCT num, pick_lng, pick_lat from distance as dis LEFT JOIN order_20200201_clear_shop on order_20200201_clear_shop.shop_id=dis.num where shop_or_package=0")
    distance$lat[(1+length(kong2$tracking_id)):length(distance$num)]<-m$pick_lat
    distance$lng[(1+length(kong2$tracking_id)):length(distance$num)]<-m$pick_lng
    n<-sqldf("select DISTINCT num, deliver_lng, deliver_lat from distance as dis LEFT JOIN order_20200201 on order_20200201.tracking_id=dis.num where shop_or_package=1")
    distance$lat[1:length(kong2$tracking_id)]<-n$deliver_lat
    distance$lng[1:length(kong2$tracking_id)]<-n$deliver_lng
    return(distance)
    #先把骑手和店铺的距离算出来
  }}

{
  ##找到最优路
  path_true<-function(b){
    courier_start_position%>%select(courier_id,courier_wave_start_lat,courier_wave_start_lng)%>%filter( . ,courier_id==b$acourier)->qishou2
    kong2<-b$shop_package  
    kong3<-unique(kong2$shop_id)
    pick_shop<-pick_shop(b)
    distance<-path_fake(b)
    shunxu<-c()
    juli<-c()
    indicater<-1
    lat<-qishou2$courier_wave_start_lat
    lng<-qishou2$courier_wave_start_lng
    conclude<-rep(Inf,length(distance$shop_or_package))
    useing<-c(1+length(kong2$tracking_id)):length(distance$distance)
    
    for(o in useing){
      conclude[o]<-let_distance(lat,lng,distance$lat[o],distance$lng[o])
    }
    while (indicater<=length(distance$distance)) {
      z<-which(conclude==min(conclude))
      if(length(z)==1){
        z<-z
      }else{
        z<-z[1]
      }
      shunxu[indicater]<-z
      juli[indicater]<-conclude[z]
      if(z<=length(kong2$tracking_id)){
        lat<-distance$lat[z]
        lng<-distance$lng[z]
        conclude<-rep(Inf,length(distance$shop_or_package))
        useing<-c(useing)[! c(useing) %in% c(z)]
        for(p in useing){
          conclude[p]<-let_distance(lat,lng,distance$lat[p],distance$lng[p])
        }
        indicater<-indicater+1
      }else{
        z1<-pick_shop[[z-length(kong2$tracking_id)]]$num
        lat<-distance$lat[z]
        lng<-distance$lng[z]
        conclude<-rep(Inf,length(distance$shop_or_package))
        useing<-c(useing,z1)[! c(useing,z1) %in% c(z)]
        for(p in useing){
          conclude[p]<-let_distance(lat,lng,distance$lat[p],distance$lng[p])
        }
        indicater<-indicater+1
      }
      
    }
    out<-list(shunxu=shunxu,juli=juli,zonglucheng=sum(juli))
    
    return(out)
  }
  #####
  #适应度函数
  functional<-function(b){
    d<-path_true(b)
    distance<-path_fake(b)
    num<-10*d$zonglucheng/(b$speed)+5*d$zonglucheng+100*d$zonglucheng/(b$speed*b$max)
    return(num)
  }
  
  zongfunction<-function(c){
    zong<-matrix(nrow=30, ncol=20)
    for(i in 1:length(c)){
      for(j in 1:length(c[[i]])){
        zong[i,j]<-functional( c[[i]][[j]] )
      }
    }
    sum<-rowSums(zong)
    return(sum)
  }
  
  
  
  #锦标赛选择
  pickbest<-function(c,sum){
    b232<-c(1:length(sum))
    kong1<-c()
    for(i in 1:20){
      a2<-which(match(sum,max(sum[sample(b232,9,replace = FALSE)]))==1)
      b232<-b232[! b232 %in% c(a2)]
      kong1[i]<-a2
    }
    return(kong1)
  }
  
  cross<-function(c,kong2,kong3){
    max_1<-c()
    for(j in 1:2){
      max_1[j]<-c[[kong2[j]]][[kong3[j]]]$max
    }
    kong4<-sample(1:(min(max_1)-2),1,replace = FALSE)
    kong6<-sample(1:(min(max_1)-kong4),1,replace = FALSE)
    kong5_shop<-c[[kong2[1]]][[kong3[1]]]$shop_package$shop_id[kong6:(kong6+kong4-1)]
    kong5_pack<-c[[kong2[1]]][[kong3[1]]]$shop_package$tracking_id[kong6:(kong6+kong4-1)]
    kong6_shop<-c[[kong2[2]]][[kong3[2]]]$shop_package$shop_id[kong6:(kong6+kong4-1)]
    kong6_pack<-c[[kong2[2]]][[kong3[2]]]$shop_package$tracking_id[kong6:(kong6+kong4-1)]
    clist<-list()
    clist[[1]]<-c[[kong2[1]]]
    clist[[2]]<-c[[kong2[2]]]
    clist[[1]][[kong3[1]]]$shop_package$shop_id[kong6:(kong6+kong4-1)]<-kong6_shop
    clist[[1]][[kong3[1]]]$shop_package$tracking_id[kong6:(kong6+kong4-1)]<-kong6_pack
    clist[[2]][[kong3[2]]]$shop_package$shop_id[kong6:(kong6+kong4-1)]<-kong5_shop
    clist[[2]][[kong3[2]]]$shop_package$tracking_id[kong6:(kong6+kong4-1)]<-kong5_pack
    return(clist)
  }
  
  zishiyingjiaocha<-function(c,sum){
    kong2<-sample(1:30,2,replace = FALSE)
    kong3<-sample(1:20,2,replace = TRUE)
    clist<-list()
    f<-max(sum[kong2])
    f_max<-max(sum)
    f_avg<-mean(sum)
    p_c<-0.3*(f_max-f)/(f_max-f_avg)
    p_c1<-0.4
    if(f>=f_avg){
      p<-p_c
    }else{
      p<-p_c1
    }
    non<-runif(1)
    if(non<p){
      clist<-cross(c,kong2,kong3)
      m1<-duplicated(clist[[1]][[kong3[1]]]$shop_package)
      m2<-duplicated(clist[[2]][[kong3[2]]]$shop_package)
      m3<-all(!m1)
      m4<-all(!m2)
      if(m3==TRUE & m4==TRUE){
        
      }else if(m3==FALSE){
        m9<-length(clist[[1]][[kong3[1]]]$shop_package$shop_id)-length(unique(clist[[1]][[kong3[1]]]$shop_package)$shop_id)
        clist[[1]][[kong3[1]]]$shop_package<-unique(clist[[1]][[kong3[1]]]$shop_package)
        
      }
      else{
        m9<-length(clist[[2]][[kong3[2]]]$shop_package$shop_id)-length(unique(clist[[2]][[kong3[2]]]$shop_package)$shop_id)
        clist[[2]][[kong3[2]]]$shop_package<-unique(clist[[2]][[kong3[2]]]$shop_package)
        
      }
    }else{
      
    }
    return(clist)
  }
  swap_1<-function(c,kong4,kong5){
    c1<-c[[kong4]][[kong5[1]]]
    c2<-c[[kong4]][[kong5[2]]]
    c1_m<-c1$max
    c2_m<-c2$max
    c1_s<-sample(1:c1_m,1)
    c2_s<-sample(1:c2_m,1)
    c1_5<-c1$shop_package$shop_id[c1_s]
    c1_6<-c1$shop_package$tracking_id[c1_s]
    c2_5<-c2$shop_package$shop_id[c2_s]
    c2_6<-c2$shop_package$tracking_id[c2_s]
    c11<-c1
    c22<-c2
    c11$shop_package$shop_id[c1_s]<-c2_5
    c11$shop_package$tracking_id[c1_s]<-c2_6
    c22$shop_package$shop_id[c2_s]<-c1_5
    c22$shop_package$tracking_id[c2_s]<-c1_6
    c1c<-c[[kong4]]
    c1c[[kong5[1]]]<-c11
    c1c[[kong5[2]]]<-c22
    clist<-list()
    clist[[1]]<-c1c
    return(clist)
  }
  
  insert_1<-function(c,kong4,kong5){
    kong2<-kong4
    kong3<-kong5
    max_1<-c()
    
    max_1[1]<-c[[kong2[1]]][[kong3[1]]]$max
    max_1[2]<-c[[kong2[1]]][[kong3[2]]]$max
    kong4<-sample(1:(min(max_1)-2),1,replace = FALSE)
    kong6<-sample(1:(min(max_1)-kong4),1,replace = FALSE)
    kong5_shop<-c[[kong2[1]]][[kong3[1]]]$shop_package$shop_id[kong6:(kong6+kong4-1)]
    kong5_pack<-c[[kong2[1]]][[kong3[1]]]$shop_package$tracking_id[kong6:(kong6+kong4-1)]
    kong6_shop<-c[[kong2[1]]][[kong3[2]]]$shop_package$shop_id[kong6:(kong6+kong4-1)]
    kong6_pack<-c[[kong2[1]]][[kong3[2]]]$shop_package$tracking_id[kong6:(kong6+kong4-1)]
    clist<-list()
    clist[[1]]<-c[[kong2[1]]]
    clist[[1]][[kong3[1]]]$shop_package$shop_id[kong6:(kong6+kong4-1)]<-kong6_shop
    clist[[1]][[kong3[1]]]$shop_package$tracking_id[kong6:(kong6+kong4-1)]<-kong6_pack
    clist[[1]][[kong3[2]]]$shop_package$shop_id[kong6:(kong6+kong4-1)]<-kong5_shop
    clist[[1]][[kong3[2]]]$shop_package$tracking_id[kong6:(kong6+kong4-1)]<-kong5_pack
    return(clist)
  }
  
  courier_change<-function(c,kong4,kong5){
    c1<-c[[kong4]]
    courior<-c()
    for(i in 1:20){
      courior[i]<-c1[[i]]$acourier
    }
    ra<-sample(1:length(courier_20200201$courier_id),1)
    new_courier<-courier_20200201$courier_id[ra]
    while(all(!duplicated(c(courior,new_courier)))!=TRUE){
      ra<-sample(1:length(courier_20200201$courier_id),1)
      new_courier<-courier_20200201$courier_id[ra]
    }
    new_courier<-courier_20200201$courier_id[ra]
    new_speed<-courier_20200201$speed[ra]
    c1[[kong5[1]]]$acourier<-new_courier
    c1[[kong5[1]]]$speed<-new_speed
    clist<-list()
    clist[[1]]<-c1
    return(clist)
  }
  
  
  zishiyingbianyi<-function(c,sum){
    kong4<-sample(1:30,1,replace = FALSE)
    kong5<-sample(1:20,2,replace = TRUE)
    clist<-list()
    newnew<-list()
    f<-max(sum[kong4])
    f_max<-max(sum)
    f_avg<-mean(sum)
    p_c<-0.2*(f_max-f)/(f_max-f_avg)
    p_c1<-0.3
    if(f>=f_avg){
      p<-p_c
    }else{
      p<-p_c1
    }
    non<-runif(1)
    if(non<p){
      po<-sample(1:3,1)
      if(po==1){
        newnew<- swap_1(c,kong4,kong5)
      }else if(po==2){
        newnew<-insert_1(c,kong4,kong5)
      }else{
        newnew<-courier_change(c,kong4,kong5)
      }
    }else{
      #该newnew判定 ifelse 长度为0 时，和长度为1
    }
    newnewnew<-list()
    if(length(newnew)==1){
      newnewnew<-newnew[[1]]
    }else{
      
    }
    
    return(newnewnew)
  }
}

{
#初始种群
a<-population(20)
c<-list()
for(i in 1 : 30){
  c[[i]]<-population(20)
}

shiying<-c()
for(i in 1:150){
  setTxtProgressBar(pb, i/100)
  paste("现在是第",i,"次")
  #sum是适应值
  sum<-zongfunction(c)
  shiying[i]<-mean(sum)
  kong1<-pickbest(c,sum)
  cnew<-list()
  cbianyi<-list()
  cjiaochao<-list()
  for (p in 1:length(kong1)) {
    cnew[[p]]<-c[[kong1[p]]]
  }
  k<-0
  for(j in 1:10){
    
    jiaocha<-zishiyingjiaocha(c,sum)
    if(length(jiaocha)==2){
      
      for(q in 1:2){
        cjiaochao[[q+k]]<-jiaocha[[q]]
        
      }
      k<-k+2
    }else{
      
    }
    
  }
  l<-0
  for(p in 1:20){
    bianyi<-zishiyingbianyi(c,sum)
    if(length(bianyi)==1){
      l<-l+1
      cbianyi[[l]]<-bianyi
    }
    cnewnew<-c(cnew,cbianyi,cjiaochao)
    length(cnewnew)
    if(length(cnewnew)==30){
      c<-cnewnew
    }else if(length(cnewnew)<30){
      yanglujia<-list()
      for(d in 1:(30-length(cnewnew))){
        a<-population(20)
        yanglujia[[d]]<-a
      }
      c<-c(cnewnew,yanglujia)
    }else{
      for(f in 1:30){
        c[[f]]<-cnewnew[[f]]
      }
    }
  }
  
  
}
show(shiying)
end_time <- Sys.time()
print(end_time-start_time)
close(pb)

plot(shiying,type="l",xlab="迭代次数",ylab="适应值")

}

a_12<-which(match(sum,max(sum))==1)
aa<-c[[a_12]]
bnm_s_lat<-c()
bnm_s_lng<-c()
bnm_p_lat<-c()
bnm_p_lng<-c()
for(ii in 1:20){
  a4<-aa[[ii]]
  a5<-path_true(a4)
  courier_start_position%>%select(courier_id,courier_wave_start_lat,courier_wave_start_lng)%>%filter( . ,courier_id==a4$acourier)->qishou_2
  lat_c<-c()
  lat_c[1]<-qishou_2$courier_wave_start_lat
  lng_c<-c()
  lng_c[1]<-qishou_2$courier_wave_start_lng
  s_id<-a4$shop_package$shop_id
  p_id<-a4$shop_package$tracking_id
  avs<-data.frame(shop=s_id,package=p_id)
  s_po<-sqldf("select pick_lng,pick_lat,shop_id from order_20200201_clear_shop as o right join avs on avs.shop=o.shop_id")
  p_po<-sqldf("select deliver_lng,deliver_lat,tracking_id from order_20200201 as o right join avs on avs.package=o.tracking_id")
  bnm_s_lat<-c(bnm_s_lat,s_po$pick_lat)
  bnm_s_lng<-c(bnm_s_lng,s_po$pick_lng)
  bnm_p_lat<-c(bnm_p_lat,p_po$deliver_lat)
  bnm_p_lng<-c(bnm_p_lng,p_po$deliver_lng)
  for(s in 1:a4$max){
   a4$shop_package$shop_po_lng[s]<-subset(s_po, s_po$shop_id==a4$shop_package$shop_id[s])$pick_lng
   a4$shop_package$shop_po_lat[s]<-subset(s_po, s_po$shop_id==a4$shop_package$shop_id[s])$pick_lat
   a4$shop_package$trac_po_lng[s]<-subset(p_po, p_po$tracking_id==a4$shop_package$tracking_id[s])$deliver_lng
   a4$shop_package$trac_po_lat[s]<-subset(p_po, p_po$tracking_id==a4$shop_package$tracking_id[s])$deliver_lat
   
    }
  final_lat<-c()
  final_lng<-c()
  zhonglei<-c()
  sd<-1
  for(l in a5$shunxu){
    if(l>a4$max){
      final_lat[sd]<-a4$shop_package$shop_po_lat[l-a4$max]
      final_lng[sd]<-a4$shop_package$shop_po_lng[l-a4$max]
      zhonglei[sd]<-1
      sd<-sd+1
    }else{
      final_lat[sd]<-a4$shop_package$trac_po_lat[l]
      final_lng[sd]<-a4$shop_package$trac_po_lng[l]
      zhonglei[sd]<-0
      sd<-sd+1
    }
  }
  #plot(bnm2$lat,bnm2$lng,col=bnm2$col,pch=20,xlim=c(38.87,39.7),ylim=c(121.5,121.7),cex=.6)
  #par(new=TRUE)
  plot(c(lat_c[1],final_lat),c(lng_c[1],final_lng),type="l",main=c("第",ii,"张"),xlim=c(38.87,39.7),ylim=c(121.5,121.7),xlab="X",ylab="Y")
 
}
 
bnm_s<-rep("2",186)
bnm_p<-rep("20",186)
bnm_s_col<-rep("red",186)
bnm_p_col<-rep("blue",186)

bnm2<-data.frame(lat=c(bnm_s_lat,bnm_p_lat),lng=c(bnm_s_lng,bnm_p_lng),col=c(bnm_s_col,bnm_p_col),tu=c(bnm_s,bnm_p))
plot(bnm2$lat,bnm2$lng,col=bnm2$col,pch=20,xlim=c(38.87,39.7),ylim=c(121.5,121.7),cex=.6)

bnm_s_lat<-c()
bnm_s_lng<-c()
bnm_p_lat<-c()
bnm_p_lng<-c()
for(ii in 1:20){
  a4<-aa[[ii]]
  a5<-path_true(a4)
  courier_start_position%>%select(courier_id,courier_wave_start_lat,courier_wave_start_lng)%>%filter( . ,courier_id==a4$acourier)->qishou_2
  lat_c<-c()
  lat_c[1]<-qishou_2$courier_wave_start_lat
  lng_c<-c()
  lng_c[1]<-qishou_2$courier_wave_start_lng
  s_id<-a4$shop_package$shop_id
  p_id<-a4$shop_package$tracking_id
  avs<-data.frame(shop=s_id,package=p_id)
  s_po<-sqldf("select pick_lng,pick_lat,shop_id from order_20200201_clear_shop as o right join avs on avs.shop=o.shop_id")
  p_po<-sqldf("select deliver_lng,deliver_lat,tracking_id from order_20200201 as o right join avs on avs.package=o.tracking_id")
  bnm_s_lat<-c(bnm_s_lat,s_po$pick_lat)
  bnm_s_lng<-c(bnm_s_lng,s_po$pick_lng)
  bnm_p_lat<-c(bnm_p_lat,p_po$deliver_lat)
  bnm_p_lng<-c(bnm_p_lng,p_po$deliver_lng)
  for(s in 1:a4$max){
    a4$shop_package$shop_po_lng[s]<-subset(s_po, s_po$shop_id==a4$shop_package$shop_id[s])$pick_lng
    a4$shop_package$shop_po_lat[s]<-subset(s_po, s_po$shop_id==a4$shop_package$shop_id[s])$pick_lat
    a4$shop_package$trac_po_lng[s]<-subset(p_po, p_po$tracking_id==a4$shop_package$tracking_id[s])$deliver_lng
    a4$shop_package$trac_po_lat[s]<-subset(p_po, p_po$tracking_id==a4$shop_package$tracking_id[s])$deliver_lat
    
  }
  final_lat<-c()
  final_lng<-c()
  zhonglei<-c()
  sd<-1
  for(l in a5$shunxu){
    if(l>a4$max){
      final_lat[sd]<-a4$shop_package$shop_po_lat[l-a4$max]
      final_lng[sd]<-a4$shop_package$shop_po_lng[l-a4$max]
      zhonglei[sd]<-1
      sd<-sd+1
    }else{
      final_lat[sd]<-a4$shop_package$trac_po_lat[l]
      final_lng[sd]<-a4$shop_package$trac_po_lng[l]
      zhonglei[sd]<-0
      sd<-sd+1
    }
  }
  plot(bnm2$lat,bnm2$lng,col=bnm2$col,pch=20,xlim=c(38.87,39.7),ylim=c(121.5,121.7),cex=.6,xlab="",ylab="")
  par(new=TRUE)
  plot(c(lat_c[1],final_lat),c(lng_c[1],final_lng),type="l",main=paste("第",ii,"张"),xlim=c(38.87,39.7),ylim=c(121.5,121.7),xlab="X",ylab="Y")
  
}

