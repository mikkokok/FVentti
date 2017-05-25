﻿open System

// Data
let ventti = 21 // Maksimipistemäärä
let jakajansuurinkasi = 17 // Kokeneet pelaajat eivät yleensä ota enempää kortteja, jos käden arvo on 17 tai suurempi.
type Maa = | Hertta = 0 | Pata = 1 | Ruutu = 2 | Risti = 3

type Numero = // Numero on discriminated union, union Numero + Maa
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

type Pelaaja = {Nimi : String; Kasi : Numero list}

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
// Funktiot 


// Sekoita korttipakka

let sekoitaPakka pakka =
    let random = new System.Random()
    pakka |> List.sortBy (fun kortti -> random.Next())

// Jaa kortti jakajalle ja pelaajalle eli aloita peli

let jaaKortti pakka = 
    match pakka with
    | kortti::loppuPakka -> (Some kortti, loppuPakka)
    | [] -> (None, [])

// Tulosta ja toimita
let main() = 
    let pakka = sekoitaPakka(Korttipakka)
    
    let jakaja = {Nimi = "Jakaja"; Kasi = List.empty}
    let pelaaja = {Nimi = "Pelaaja"; Kasi = List.empty}

    //jakaja.Kasi = List.append(jaaKortti(pakka))
    //jakaja.Kasi = List.append(jaaKortti(pakka))
    //pelaaja.Kasi = List.append(jaaKortti(pakka))
    //pelaaja.Kasi = List.append(jaaKortti(pakka))

    for kortti in pakka do 
    printf "%O \n" kortti

    //Console.WriteLine(korttipakka);
main()