#Hafta - 4: 21.03.2024 / Platformlar, R Temel Bilgiler ve Veri Özetleme (Teslim: 27.03.24)
_______________
    #Unutulmaması gerekenler:
#  ==  equals to 
#  !  not/negation
#  > veya < greater than veya less than
#  >= or <= greater than or equal to / less than or equal to
#  &  AND
#  |  OR
_______________

getwd()
küp <- function(x) {
  return(x^3) #fonksiyon yapma örneği, burada küp hesabı için yazdık
}
print(küp(5)) #küp almak için yarattığımız bir fonksiyon

#dersteki gerisayım fonksiyonu örneği
gerisayım = function(adan) {
  print(adan)
  while (adan!=0){
    Sys.sleep(1)
    adan=adan-1
    print(adan)
  }
}
gerisayım(10)

#-----

veri= read.csv("http://johnmuschelli.com/intro_to_r/data/Youth_Tobacco_Survey_YTS_Data.csv")
veri2= read.csv("C:/Users/Meli/Downloads/Youth_Tobacco_Survey__YTS__Data.csv") # komut işe yaramayacaktır zira şu anda "getwd" yaptığımızda da göreceğiz ki, çalışma alanı kendi bilgisayarımız değildir, bir sunucudadır.
getwd() #çalıştığımız dizini öğrenmek
setwd() #çalıştığımız dizin adresini değiştirmek için kullanılır

View(veri) #veriyi geniş bir şekilde görmek için
head(veri) #verinin ilk 6 satırını almak

dim(veri) #sırasıyla satır ve sütun sayısı için kullanılıyoruz
nrow (veri) #"veri" için row sayısı
ncol (veri) #"veri" için column sayısı

help("dim") #help ile bir komutun içeriği hakkında bilgi alabiliriz.

install.packages("dplyr")
library("dplyr")
install.packages("readr")
library("readr")

veri_yeniden_ad =rename(veri, year=YEAR)
names(veri)
names(veri_yeniden_ad) #eski ve yeni verilere bakarsak year'ı YEAR'e çevirdiğimizi görebiliriz.

veri_yendien_ad2= rename(veri_yeniden_ad, Year=year)
write_csv("veri_yendien_ad2, path=tutun.csv")

ls() #son yaptıklarımız

install.packages("tidyverse")
library(tidyverse)

veri2=data.frame(mtcars) #mtcars'ı direkt data(mtcars) olarak çağırırsak sadece karakter olarak görüyor, bu bir sıkıntı yaratır
dim(mtcars)
view(veri2) #mtcars'ı veri2 olarak aldığımızı onaylayabiliriz içeriğe bakarak

veri2_tibble=as_tibble(veri2) #daha önce veri2 olarak tanımladığımızı tibble olarak ele alacağız bu şekilde
head(veri2_tibble) #ilk 6 satırı görelim

veri2_yeniden_ad=dplyr::rename(veri2_tibble, MPG=mpg) #dplyr:rename ile bir değişken ismini değiştirdik
veri2_yeniden_ad2 = dplyr::rename_all(veri2_yeniden_ad, toupper) #tüm değişken isimleri büyük harf ile yazdırdık

names(veri2) #verideki değişken isimlerini görürüz
veri2_gear = veri2$gear # veri2_gear adında bir veri seti oluşturduk, bu set veri2'de bulunan gear verilerinden oluşmakta

dim(veri2_gear) #null cevabı alıyoruz şimdi, çünkü class'ı bu işleme uygun değil
?`matrix-class`

veri2_mpg=select(veri2,mpg) #veri2_mpg olarak bir veri oluşturduk, veri2'deki mpg değişkeninden oluşuyor tamamı
dim(veri2_mpg)
veri2_mpg2=pull(select(veri2, mpg)) #böyle yazarsak numerik olarak tutar veriyi, values olarak
dim(veri2_mpg2) #vektör olarak tanımlanmadığı için NULL veriyor

veri2_mpg_fil = filter(veri2, mpg > 19 | mpg < 17) #veriyi filtreleme örneği
veri2_gear = select (filter(veri2, mpg > 19 | mpg <17), gear) #kısaltma kullandık, üstteki satırdaki datayı direkt göstererek
veri2_gear=select(veri2_mpg_fil, gear) # 19'dan büyük ve 17'den küçük mpg verisi + gear bilgisi

veri2_piped= veri2 %>% filter(mpg >22 & cyl==4) #ilk koşul %>% select(disp, qsec) #ikinci koşul #  pipe bastık, arka arkaya işlemleri sıraladık

veri2$yenisütun = veri2$wt*3 #veri2'ye yenisütun adı altında bir sütun ekledik ve içeriğini wt'nin 3 üssü olarak belirledik
view(veri2) #fakat burada unutmayalım ki, üstteki kodu bastıktan sonra orijinal olan veri2'yi bozduk o halde yeniden veri2=mtcars yazarak yeniden orijinal dosyayı çağıralım
veri2=mtcars
view(veri2)

veri2_mut = mutate(veri2, newcol=wt*3) #veri2_mut adı altıdna yeni bir değişken açtık ve içerisine newcol adlı bir değişken ekledik. bunun içeriği ise wt değerini 3 ile çarparak doldurduk
view(veri2_mut)

veri2_mut2= mutate(veri2, disp_kateg= ifelse(disp <200, "düşük", ifelse(disp <=400,
                                                                        "orta",
                                                                        "yüksek"
                                                                        )))
#disp, 200'den küçükse düşük, 400 ve 400'den küçükse orta, daha yüksek ise yüksek yazacak değer olarak.
view(veri2_mut2)

arrange(veri2, desc(disp)) #sıralama yaptık, descending olarak

transmute(veri2, newcol=disp*5, mpg, gear) #newcol olarak yeni bir değişken geldi, bunun da içeriği disp'in 5 ile çarpımı oldu. diğerleri de aynı kaldı.


# Şimdiyse bu veriler üzerinden amaca yönelik kısa bir çalışma yapmanın iyi olacağı kanaatindeyim. Çalışmam, belirli bir araç alma yönünde araştırma sürecini örnekleyecek.

install.packages("tidyverse")
library(tidyverse)

arac_listesi=data.frame(mtcars) #ilk olarak araç listesini ekledim. şimdiyse bu araç listesinden ihtiyacıma yönelik bir araç bulmak için çeşitli filtrelemeler yapacağım.

view(arac_listesi)

class(arac_listesi) #data.frame olduğunu görüyorum.

arac_vites_sayisi = select (filter(arac_listesi, gear > 3 | mpg <6), gear) #kısaltma kullandık, üstteki satırdaki datayı direkt göstererek. Amacım sadece 5 vitesli araçları görmekti ve bunu bu data içerisinde görebiliriz.

arac_listesi_pipe = arac_listesi %>% filter(hp >200 & cyl== 5) #şimdi ise sadece 200hp'den yüksek ve aynı zamanda 5 silindirli bir araç aradığımda hiçbir şey bulamadığımı görüyorum.

arac_listesi_uygunluk= mutate(arac_listesi, hp_kateg= ifelse(hp <149 & cyl<5 , "Satın Alınmamalı", ifelse(hp > 150 & cyl>5, "Satın Alınmalı", "Satın Alınmamalı"))) # Şimdi ise, satın alınmak için belirli bir 
#Üstteki satırda ise bir araç alma durumunda kısa bir filtreleme örneği gerçekleştirdim. Hp 149'dan düşük ve silindir sayısı 5'ten düşük ise "Satın Alınmamalı" sonucunu aldık; Hp 150'den büyük ve 5 silindirden büyükse satın alınmalı mesajını aldık. Böylece amaca yönelik ksıa bir örnek gerçekleştirdik.
