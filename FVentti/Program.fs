open System

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

type Pelaaja = {
                Nimi : String;
                Kasi : Numero list;
                }

// Luo korttipakka
// Korttipakka on Numero list
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
// %A discminited unioneille
// Kerro pakka
let kerroPakka pakka =
    printf "----------Korttipakan kortit--------------------------- \n"
    for kortti in pakka do 
    printf "%A \n" kortti

// Kerro pelaajan käsi
let kerroPelaaja pelaaja = 
    printf "----------Pelaajan kortit--------------------------- \n"
    for kortti in pelaaja do
    printf "%A \n" kortti

// Kerro jakajan käsi
let kerroJakaja jakaja =
    printf "----------Jakajan kortit--------------------------- \n"
    for kortti in jakaja do
    printf "%A \n" kortti
// Sekoita korttipakka

let sekoitaPakka pakka =
    let random = new System.Random()
    pakka |> List.sortBy (fun kortti -> random.Next())

// Jaa kortti jakajalle ja pelaajalle eli aloita peli
// Palauttaa tuplen (kortti, pakka)
let jaaKortti pakka = 
    match pakka with
    | kortti::loppuPakka -> (Some kortti, loppuPakka)
    | [] -> (None, [])

let annaKorttiPakasta pakka = 
    Korttipakka.Head


// Tulosta ja toimita
let main() = 
    let pakka = sekoitaPakka(Korttipakka)

    let jakaja = {Nimi = "Jakaja";  Kasi = List.empty}
    let pelaaja = {Nimi = "Pelaaja"; Kasi = List.empty}
    kerroPakka pakka // Lukee pakan

    // Testausta varten
    let (kortti, pakka) = jaaKortti pakka
    let (korttia, pakka) = jaaKortti pakka
    let (korttib, pakka) = jaaKortti pakka
    let (korttic, pakka) = jaaKortti pakka
    let (korttid, pakka) = jaaKortti pakka  
    
    printf "%A \n" kortti
    printf "%A \n" korttia
    printf "%A \n" korttib
    printf "%A \n" korttic
    printf "%A \n" korttid
    printf "------------------------------------------------ \n"
    kerroPakka pakka // Lukee pakan
    
    

    
    
    //jakaja.Kasi = List.append(jaaKortti(pakka))
    //jakaja.Kasi = List.append(jaaKortti(pakka))
    //pelaaja.Kasi = List.append(jaaKortti(pakka))
    //pelaaja.Kasi = List.append(jaaKortti(pakka))


    
    






    //Console.WriteLine(korttipakka);
main()