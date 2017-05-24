open System

// Data
let ventti = 21 // Maksimipistemäärä
let jakajansuurinkasi = 17 // Kokeneet pelaajat eivät yleensä ota enempää kortteja, jos käden arvo on 17 tai suurempi.
type Maa = | Hertta = 0 | Pata = 1 | Ruutu = 2 | Risti = 3

type Numero = 
| Assa of Maa  // ässä on 1 tai 14 (pelaaja valitsee kumpaa arvoa haluaa kulloinkin käyttää).
| Kaksi of Maa  
| Kolme of Maa 
| Nelja of Maa 
| Viisi of Maa 
| Kuusi of Maa 
| Seitseman of Maa 
| Kahdeksan of Maa 
| Yhdeksan  of Maa
| Kymmenen of Maa 
| Jatka of Maa 
| Kuningatar of Maa 
| Kuningas of Maa

// Luo korttipakka
// Korttipakka on Kortti list
let Korttipakka = [ for maa in [Maa.Hertta; Maa.Pata; Maa.Ruutu; Maa.Risti] do
                    yield Assa(maa)
                    yield Kaksi(maa)
                    yield Kolme(maa)
                    yield Nelja(maa)
                    yield Viisi(maa)
                    yield Kuusi(maa)
                    yield Seitseman(maa)
                    yield Kahdeksan(maa)
                    yield Yhdeksan(maa)
                    yield Kymmenen(maa)
                    yield Jatka(maa)
                    yield Kuningatar(maa)
                    yield Kuningas(maa)
                   ]



// Sekoita korttipakka


// Jaa kortti jakajalle ja pelaajalle eli aloita peli


// Tulosta ja toimita
let main() = 
    for kortti in Korttipakka do 
    printf "%O \n" kortti
    //Console.WriteLine(korttipakka);
main()