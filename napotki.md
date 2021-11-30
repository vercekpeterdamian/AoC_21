# Advent of code 2020, 2021, 

## Nasveti za uporabo

V datoteki `src/solutions/solution.ml` je definiran glavni program, ki sprejme številko dneva `X` prek ukazne vrstice, pokliče funkciji `naloga1`, `naloga2` definirani v modulu `src/dayX.ml` na vhodnih podatkih `data/day_X.in` in njuna rezultata izpiše na standardni izhod ter datoteki `out/day_X_1.out` in `out/day_X_2.out`.

Izhod, ki ga proizvede program šteje kot veljaven za dodatne točke.

Primer uporabe je tako za `opam`, kot brez njega podan za dan `0`, ki je rešitev prvega dneva iz leta 2019.

## Uporaba z nameščenim `opam`-om

Namestite si ukaz `make` in z opamom namestite `dune utop ocamlformat`.

Rešitev dneva `X` najlažje podate tako, da v mapo `src/solutions/` dodate datoteko `dayX.ml`. V tej datoteki definirajte nov modul `Solver` in funkciji `naloga1` ter `naloga2`. Po želji si lahko definirate tudi pomožne funkcije. Če pa te funkcije pridejo v uporabo pogosteje, se vam jih splača definirati kje v `utils`, kot so recimo definirani `list_utils`.

V datoteki `solution.ml` dodajte nov vzorec v funkcijo `choose_solver`. Vhodne podatke zapišite v datoteko `data/day_X.in`.

Program sedaj poženete z `make run DAY=X`. Program bo prebral vsebino vhodne datoteke za dan X, pognal `naloga1` in `naloga2` ter njuna rezultata izpisal tako na standardni izhod kot v ustrezni izhodni datoteki.

## Uporaba brez nameščenega `opam`-a (sistem windows brez dockerja)

V datoteki `project_windows` je že pripravljena koda za reševanje v eni sami datoteki. 

Za reševanje popravite funkcijo `choose_solver` in dodajte ustrezen modul `SolverX`.

Program poženite s popravljenim `task`-om iz repozitorija predmeta. POZOR: Odstranite `-init`, saj drugače testni program ne prepozna argumenta X:

```bash
C:\\ ... bin\\ocaml.exe  \"${file}\" X"
```

Kjer je X številka dneva, ki ga rešujete.

## Pogosta vprašanje in rešitve

### Ocaml javlja napako `end of line sequence`/`end of file` pri branju datoteke

Datoteka je shranjena v `CRLF` formatu, pretvorite jo format `LF`. To najlažje storite tako, da jo odprete v VSC in spodaj izberete pravilni format ter ponovno shranite datoteko.

Natančnejša navodila je Žiga prijazno napisal na <https://ucilnica2021.fmf.uni-lj.si/mod/forum/discuss.php?d=18334>

### Funkcija `int_of_string` javi napako

Pri kopiranju vsebine datoteke ste verjetno po nesreči dodali še eno prazno vrstico na konec. Ocaml tako skuša prazen niz pretvoriti v številko, kar pa ga zmoti. Odstranite prazno vrstico s konca vhodne datoteke.
