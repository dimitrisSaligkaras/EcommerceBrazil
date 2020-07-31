#Case study:Brazilian Ecommerce"

  
#Ερώτηση:Να γράψετε κώδικα που να βρίσκει από πόσοι πελάτες της επιχείρησης έχουν δηλώσει τοποθεσία το Ρίο ντε Τζανέιρο

library("haven")
library("dplyr")
library("readr")
olist_customers_dataset%>%
  filter(customer_city=="rio de janeiro")%>%
  select(customer_unique_id)%>%
  group_by(customer_unique_id)%>%
  summarise(k=sum(n()))%>%
  arrange(k)





#Ερώτηση:Να βρείτε το όνομα του πελάτη που πραγματοποίησε σε μία παραγγελία την αγορά των περισσότερων προϊόντων

a=full_join(olist_orders_dataset,olist_order_items_dataset,by="order_id")
a%>%
  select(customer_id,order_item_id)%>%
  arrange(desc(order_item_id)) 



#Ερώτηση:Να υπολογίσετε το συνολικό κόστος (αγορα+μεταφορικά) των προϊόντων που αγοράστηκαν από το Sao Paolo


a1=full_join(a,olist_customers_dataset,by="customer_id")
a1$synoliko_kostos=with(a1,freight_value+price)
a1%>%
  filter(customer_city=="sao paulo",order_status=="delivered")%>%
  select(synoliko_kostos)%>%
  summarise(ord=n())%>%
  mutate(ord)%>%
  slice(1)

#isodynama
a1=full_join(a,olist_customers_dataset,by="customer_id")
a1$synoliko_kostos=with(a1,freight_value+price)
a1%>%
  filter(customer_city=="sao paulo",order_status=="delivered")%>%
  select(synoliko_kostos)%>%
  tally()%>%
  arrange(n)%>%
  slice(1)



#Ερώτηση:Να βρείτε τον τρόπο/τρόπους πληρωμής του πελάτη από το Ρίο ντε Τζανέιριο που είχε τις μεγαλύτερες από πλευράς κόστους αγορές

a3=full_join(olist_customers_dataset,olist_orders_dataset,by="customer_id")

a4=full_join(a3,olist_order_payments_dataset,by="order_id")

a4%>%
  filter(customer_city=="rio de janeiro")%>%
  select(customer_unique_id,payment_type,payment_value)%>%
  group_by(customer_unique_id)%>%
  summarise(v=sum(payment_value))%>%
  arrange(desc(v))%>%
  slice(1)



#opote mehrh edv vrhkame ton pelath apo to rio de janeiro pou ekane tis megaluteres agores se #xrhmatikh #axia na einai o 0a0a92112bd4c708ca5fde585afaa872

#twra tha vrume me poio tropo ekane tiw plhromes tou

a4%>%
  filter(customer_unique_id=="0a0a92112bd4c708ca5fde585afaa872")%>%
  select(payment_type)%>%
  group_by(payment_type)%>%
  tally()%>%
  arrange(desc(n))



#Ερώτηση:Να βρείτε σε ποιον μήνα πραγματοποιήθηκε η μεγαλύτερη εισροή εσόδων για την επιχείρηση

olist_orders_dataset=olist_orders_dataset %>% mutate(year=year(order_purchase_timestamp))
olist_orders_dataset=olist_orders_dataset %>% mutate(month=month(order_purchase_timestamp))
olist_orders_dataset=olist_orders_dataset %>% mutate(day=day(order_purchase_timestamp))
olist_orders_dataset=olist_orders_dataset %>% mutate(week=week(order_purchase_timestamp))
a14=full_join(olist_products_dataset,olist_order_items_dataset,by="product_id")
a15=full_join(a14,olist_orders_dataset,by="order_id")
#dhmiourgia kainourias sthlhs me ta esoda apo kathe paragelia
a15$esoda=with(a15,order_item_id*price)

a15%>%
  filter(order_status=="delivered")%>%
  select(order_item_id,month,price,esoda)%>%
  group_by(month)%>%
  summarise(k=sum(esoda))%>%
  arrange(desc(k))




#Ερώτηση:Να εμφανίσετε την κατηγορία προϊόντος για κάθε έ
#να από τα προϊόντα που δεν παραδόθηκαν και που δεν απεστάλθηκαν

a16=full_join(olist_products_dataset,olist_order_items_dataset,by="product_id")
a17=full_join(a16,olist_orders_dataset,by="order_id")
a17%>%
  filter(order_status!="delivered")%>%
  select(product_category_name)%>%
  group_by(product_category_name)



#Ερώτηση: Ποια ήταν η πόλη του πιο δημοφιλούς(αριθμός πωλήσεων) seller;
a5=full_join(olist_sellers_dataset,olist_order_items_dataset,by="seller_id")
a5%>%
  select(order_item_id,seller_id,seller_city)%>%
  group_by(seller_id,seller_city)%>%
  summarise(k=sum(order_item_id))%>%
  arrange(desc(k))



#Ερώτηση:Να βρείτε τα δύο προϊόντα με τις μεγαλύτερες σε πλήθος πωλήσεις που είχαν περισσότερες από 3 φωτογραφίες στην περιγραφή τους

a13=full_join(olist_products_dataset,olist_order_items_dataset,by="product_id")
a13%>%
  filter(product_photos_qty>3)%>%
  select(order_item_id,product_category_name)%>%
  group_by(product_category_name)%>%
  summarise(k=sum(order_item_id))%>%
  arrange(desc(k))%>%
  slice(1,2)



#Ερώτηση:Να βρείτε την κατηγορία προϊόντων που είχε τον καλύτερο μέσο όρο αξιολογήσεων

a10=full_join(olist_order_reviews_dataset,olist_orders_dataset,by="order_id")
a11=full_join(olist_products_dataset,olist_order_items_dataset,by="product_id")
a12=full_join(a10,a11,by="order_id")
a12%>%
  select(review_score,product_category_name)%>%
  group_by(product_category_name)%>%
  summarise(k=mean(review_score))%>%
  arrange(desc(k))

#Ερώτηση: Να υπολογίσετε το εβδομαδιαίο έσοδο αγορών της κατηγορίας esporte_lazer για τους Καλοκαιρινούς μήνες 



install.packages("lubridate")
install.packages("xts")
library(lubridate)
library(xts)
olist_orders_dataset=olist_orders_dataset %>% mutate(year=year(order_purchase_timestamp))
olist_orders_dataset=olist_orders_dataset %>% mutate(month=month(order_purchase_timestamp))
olist_orders_dataset=olist_orders_dataset %>% mutate(day=day(order_purchase_timestamp))
olist_orders_dataset=olist_orders_dataset %>% mutate(week=week(order_purchase_timestamp))
a14=full_join(olist_products_dataset,olist_order_items_dataset,by="product_id")
a15=full_join(a14,olist_orders_dataset,by="order_id")
a15%>%
  filter(month %in% c(6,7,8)& product_category_name=="esporte_lazer"&order_status=="delivered")%>%
  select(order_item_id,month,price)%>%
  group_by(month)%>%
  summarise(k=sum((order_item_id*price)/7))%>%
  arrange(k)


#Ερώτηση:Να βρείτε σε ποια κατηγορία προϊόντος αγοράζουν περισσότερο με την χρήση του τρόπου boleto στην περιοχή MG

a6=full_join(olist_order_payments_dataset,olist_orders_dataset,by="order_id")
a9=full_join(olist_customers_dataset,a6,by="customer_id")
a7=full_join(olist_products_dataset,olist_order_items_dataset,by="product_id")
a8=full_join(a9,a7,by="order_id")
a8%>%
  filter(payment_type=="boleto",customer_state=="MG")%>%
  select(product_category_name,order_item_id)%>%
  group_by(product_category_name)%>%
  summarise(k=sum(order_item_id))%>%
  arrange(desc(k))



