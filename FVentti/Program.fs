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
let kerroPelaaja kasi = 
    printf "----------Pelaajan kortit--------------------------- \n"
    for kortti in kasi do
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
// Palauttaa tuplen (kortti option, pakka)
// Jos pakka on tyhjä palauttaa optionin ja tyhjän pakan
let jaaKortti pakka = 
    match pakka with
    | kortti::loppuPakka -> (Some kortti, loppuPakka)
    | [] -> (None, [])

// Aseta kortti käteen
let korttiKateen kortti kasi =
    let ret = kasi :: kortti
    ret

// Funktio jolla kysytään pelaako pelaaja vielä
let nostaakoKortin =
    printf "Nostatko kortin? Vastaa k tai e"
    let ret = Console.ReadLine()
    if ret = "k" then true
    else false
// Funktio jolla lasketaan kädessä olevien korttien arvot
let laskeKortit kasi = 
    printf "Lasketaan korttien summa"
    for kortti in kasi do
    kortti


// Tulosta ja toimita
let main() = 
    let pakka = sekoitaPakka(Korttipakka)

    let mutable jakaja = {Nimi = "Jakaja";  Kasi = []}
    let mutable pelaaja = {Nimi = "Pelaaja"; Kasi = List.empty}
    kerroPakka pakka // Lukee pakan
    // Testausta varten
    let (kortti, pakka) = jaaKortti pakka
    let (korttia, pakka) = jaaKortti pakka
    let (korttib, pakka) = jaaKortti pakka
    let (korttic, pakka) = jaaKortti pakka
    let (korttid, pakka) = jaaKortti pakka  
    let lista = [kortti.Value; korttia.Value; korttib.Value]
    printf "\n\n %A \n\n" lista.Length 
    let lista = List.append pelaaja.Kasi lista
    let player = {pelaaja with Kasi = lista}
    //pelaaja.Kasi = korttiKateen pelaaja.Kasi kortti.Value
    //let testi = kortti.Value :: pelaaja.Kasi
    printf "\n\n %A \n\n" player.Kasi.Length 
    let numero = haeKortinNumero kortti.Value
    printf "\n\n %A \n\n" numero


    printf "%A \n" kortti.Value
    printf "%A \n" korttia.Value
    printf "%A \n" korttib.Value
    printf "%A \n" korttic
    printf "%A \n" korttid
    printf "------------------------------------------------ \n"
    kerroPakka pakka // Lukee pakan
    printf "------------------------------------------------ \n"
    kerroPelaaja player.Kasi

    
    //jakaja.Kasi = List.append(jaaKortti(pakka))
    //jakaja.Kasi = List.append(jaaKortti(pakka))
    //pelaaja.Kasi = List.append(jaaKortti(pakka))
    //pelaaja.Kasi = List.append(jaaKortti(pakka))
    //Console.WriteLine(korttipakka);
main()