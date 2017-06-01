open System
open Microsoft.FSharp.Reflection

// Data
let ventti = 21 // Maksimipistemäärä
let jakajansuurinkasi = 17 // Kokeneet pelaajat eivät yleensä ota enempää kortteja, jos käden arvo on 17 tai suurempi.
type Maa = | Hertta = 0 | Pata = 1 | Ruutu = 2 | Risti = 3

type Kortti = // Kortti on discriminated union, union Numero + Maa
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

let haeKortinNumero kortti =
    match kortti with
    | Assa(_) -> 1 // tai 14
    | Kaksi(_) -> 2
    | Kolme(_) -> 3
    | Nelja(_) -> 4
    | Viisi(_) -> 5
    | Kuusi(_) -> 6
    | Seitseman(_) -> 7
    | Kahdeksan(_) -> 8
    | Yhdeksan(_) -> 9
    | Kymmenen(_) -> 10
    | Jatka(_) -> 11
    | Kuningatar(_) -> 12
    | Kuningas(_) -> 13

type Pelaaja = {
                Nimi : String;
                Kasi : Kortti list;
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
let kerroPelaaja kasi korttienSumma = 
    printf "----------Pelaajan kortit--------------------------- \n"
    printf "%A \n" korttienSumma
    for kortti in kasi do
    printf "%A \n" kortti 

// Kerro jakajan käsi
let kerroJakaja jakaja korttienSumma =
    printf "----------Jakajan kortit--------------------------- \n"
    printf "%A \n" korttienSumma
    for kortti in jakaja do
    printf "%A \n" kortti
// Sekoita korttipakka

let sekoitaPakka pakka =
    let random = new System.Random()
    pakka |> List.sortBy (fun kortti -> random.Next())


// Jaa kortti jakajalle ja pelaajalle eli aloita peli
// Palauttaa tuplen (kortti option, pakka)
// Jos pakka on tyhjä palauttaa optionin ja tyhjän pakan
let jaaKortti pakka = 
    match pakka with
    | kortti::loppupakka -> (Some kortti, loppupakka)
    | [] -> (None, [])    

// Aseta kortti käteen
let korttiKateen kortti kasi =
    let ret = kasi :: kortti
    ret

let add x y =
    x + y

// Funktio jolla kysytään pelaako pelaaja vielä
let nostaakoKortin () =
    printf "Nostatko kortin? Vastaa k tai e"
    let ret = Console.ReadLine()
    if ret = "k" then true
    else false
// Funktio jolla lasketaan kädessä olevien korttien arvot
let laskeKortit kasi = 
    printf "Lasketaan korttien summa"
    let mutable ret = 0
    for kortti in kasi do
    ret <- ret + haeKortinNumero kortti
    done
    ret

let onkoVentti korttienSumma = 
    if korttienSumma = ventti then true
    else  false

let tarkistaKierroksenVoitto korttienSumma = 
    let ventti = onkoVentti korttienSumma
    if ventti then true
    else false


let pelaaPelia pelaaja jakaja pakka = 
    let mutable pelaakoPelaaja = nostaakoKortin()
    let mutable pelaaja = pelaaja
    let mutable jakaja = jakaja
    let mutable pakka = pakka
    let mutable pelaakoJakaja = true
    
    while pelaakoPelaaja do
        let (nostettuKortti, loppuPakka) = jaaKortti pakka
        pelaaja <- {Nimi = "Pelaaja"; Kasi = korttiKateen pelaaja.Kasi nostettuKortti.Value}
        let korttienSumma = laskeKortit pelaaja.Kasi
        kerroPelaaja pelaaja.Kasi korttienSumma
        let onkoVoitto = tarkistaKierroksenVoitto korttienSumma
        if onkoVoitto then
            pelaakoPelaaja <- false
        else
            pakka <- loppuPakka
            pelaakoPelaaja <- nostaakoKortin() 

    while pelaakoJakaja do
        let (nostettuKortti, loppuPakka) = jaaKortti pakka
        let kortti = [nostettuKortti.Value]
        jakaja <- {Nimi = "Jakaja"; Kasi = korttiKateen jakaja.Kasi nostettuKortti.Value}
        let korttienSumma = laskeKortit jakaja.Kasi
        kerroJakaja jakaja.Kasi korttienSumma
        pakka <- loppuPakka
    

// Tulosta ja toimita
let main() = 
    let pakka = sekoitaPakka(Korttipakka)   
    //kerroPakka pakka // Lukee pakan

    //Vedetään pelaajan ja jakajan 2 ekaa korttia pakasta
    let (pelaajanEkaKortti, pakka) = jaaKortti pakka
    let (pelaajanTokaKortti, pakka) = jaaKortti pakka
    let (jakajanEkaKortti, pakka) = jaaKortti pakka
    let (jakajanTokaKortti, pakka) = jaaKortti pakka

    //Luodaan molemmille lista vedetyistä korteista
    let pelaajanAloitusKortit = [pelaajanEkaKortti.Value; pelaajanTokaKortti.Value]
    let jakajanAloitusKortit = [jakajanEkaKortti.Value; jakajanTokaKortti.Value]

    //Luodaan pelaaja ja jakaja sekä asetetaan vedetyt kortit käteen
    let jakaja = {Nimi = "Jakaja";  Kasi = pelaajanAloitusKortit}
    let pelaaja = {Nimi = "Pelaaja"; Kasi = jakajanAloitusKortit}

    let pelaajanAloitusKadenSumma = laskeKortit pelaajanAloitusKortit
    let jakajanAloitusKadenSumma = laskeKortit jakajanAloitusKortit

    kerroPelaaja pelaaja.Kasi pelaajanAloitusKadenSumma
    kerroJakaja jakaja.Kasi jakajanAloitusKadenSumma

    pelaaPelia pelaaja jakaja pakka

    // Testausta varten
    //printf "\n\n %A \n\n" pelaajanAloitusKortit.Length 
    //let lista = List.append pelaaja.Kasi pelaajanKortit
    //pelaaja.Kasi = List.append pelaaja.Kasi pelaajanKortit

    //let pelaajankasi = {pelaaja with Kasi = lista}
    //pelaaja.Kasi = korttiKateen pelaaja.Kasi kortti
    //let testi = pelaajanEkaKortti :: pelaaja.Kasi
    //printf "\n\n %A \n\n" pelaajankasi.Kasi.Length 
    //let numero = haeKortinNumero pelaajanEkaKortti.Value
    //printf "\n\n %A \n\n" numero


    //printf "%A \n" kortti.Value
    //printf "%A \n" korttia.Value
    //printf "%A \n" korttib.Value
    //printf "%A \n" korttic
    //printf "%A \n" korttid
    //printf "------------------------------------------------ \n"
    //kerroPakka pakka // Lukee pakan
    //printf "------------------------------------------------ \n"
    kerroPelaaja pelaaja.Kasi

    
    ///jakaja.Kasi = List.append(jaaKortti(pakka))
    ///jakaja.Kasi = List.append(jaaKortti(pakka))
    ///pelaaja.Kasi = List.append(jaaKortti(pakka))
    ///pelaaja.Kasi = List.append(jaaKortti(pakka))
    ///Console.WriteLine(korttipakka);
main()