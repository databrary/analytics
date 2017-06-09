PIs-Volumes-Sessions-Hrs
================
Rick O. Gilmore
2017-06-09 08:22:34

Purpose
-------

Summarize Databrary usage as of ~Feburary 2017.

Preliminaries
-------------

Import data
-----------

``` r
pis.vols.sess.hrs <- read.csv("analysis/csv/pi.vols.sess.hrs.csv")
```

By Number of Volumes
--------------------

``` r
pis.vols.sess.hrs %>%
  select(pi, institution, vols, sessions, hrs) %>%
  arrange(desc(vols)) %>%
  knitr::kable()
```

| pi                       | institution                            |  vols|  sessions|        hrs|
|:-------------------------|:---------------------------------------|-----:|---------:|----------:|
| Adolph Karen             | New York University                    |    38|      8507|  1916.5497|
| Gilmore Rick O.          | The Pennsylvania State University      |    15|      1100|   112.5792|
| Weisberg Deena Skolnick  | University of Pennsylvania             |    12|      1058|    55.6356|
| Frank Michael C.         | Stanford University                    |     8|       454|    80.1053|
| Vishton Peter            | College of William and Mary            |     7|       214|    13.2639|
| Smith Linda B.           | Indiana University                     |     5|      1116|   114.8075|
| Tamis-LeMonda Catherine  | New York University                    |     5|      9835|  1297.1506|
| Foo Vicky                | New York University                    |     4|       166|    11.3847|
| Gelman Susan             | University of Michigan                 |     4|         0|     0.0000|
| Needham Amy              | Vanderbilt University                  |     4|       292|    23.5197|
| Woodward Amanda          | University of Chicago                  |     4|       186|    30.4769|
| Amso Dima                | Brown University                       |     3|         6|     0.0256|
| Bertenthal Bennett I.    | Indiana University                     |     3|        68|     0.2317|
| Brannon Elizabeth M      | Duke University                        |     3|        12|     0.0928|
| DeLoache Judy            | University of Virginia                 |     3|        16|     0.0636|
| Fabricius William        | Arizona State University               |     3|       172|    16.3872|
| James Karin              | Indiana University                     |     3|        34|     7.5094|
| Jayaraman Swapnaa        | Indiana University                     |     3|       994|   110.8747|
| Jimenez-Robbins Carmen   | None                                   |     3|       150|    17.8336|
| Raver Cybele             | New York University                    |     3|      1604|    70.3700|
| Staff None               | Databrary                              |     3|        89|    34.6514|
| Alibali Martha W.        | University of Wisconsin - Madison      |     2|        16|     3.7600|
| Blair Clancy             | New York University                    |     2|      1594|    69.6831|
| Cordes Sara              | Boston College                         |     2|         0|     0.0000|
| Fausey Caitlin M.        | University of Oregon                   |     2|         8|     0.0667|
| Franchak John            | None                                   |     2|        58|     0.7714|
| Goldstein Thalia R.      | Pace University                        |     2|        52|    10.3194|
| Heathcock Jill           | Ohio State University                  |     2|        96|     6.5678|
| Karasik Lana             | CUNY                                   |     2|       973|   405.8867|
| Messinger Daniel         | University of Miami                    |     2|       214|    19.7519|
| Oakes Lisa               | UC Davis                               |     2|       404|     7.3550|
| Pfautz Anna              | University of Chicago                  |     2|       186|    30.4769|
| Professor Suzanne Q.     | Databrary                              |     2|         8|     0.0431|
| Raudies Florian          | Boston University                      |     2|      1010|   110.8944|
| Swift Jackie             | Arizona State University               |     2|        34|     3.5067|
| Waxman Sandy             | Northwestern University                |     2|         2|     0.0950|
| Wilkinson Krista         | Pennsylvania State University          |     2|         2|     0.0014|
| Baker David              | University of Akron                    |     1|       356|    16.7089|
| Barr Rachel              | Georgetown University                  |     1|         0|     0.0000|
| Bergelson Elika          | University of Rochester                |     1|         8|     4.0958|
| Berthier Neil            | University of Massachusetts Amherst    |     1|         2|     0.0842|
| Blumberg Mark            | University of Iowa                     |     1|        14|     6.5994|
| Boerger Elizabeth        | Slippery Rock University               |     1|       178|    19.5969|
| Brand Rebecca            | Villanova University                   |     1|        16|     0.6953|
| Brandone Amanda          | Lehigh University                      |     1|        68|     8.6200|
| Bunce Louise             | University of Winchester               |     1|       178|    19.5969|
| Burdick Jessica          | New York University                    |     1|        10|     0.6869|
| Buss Kristin A           | Pennsylvania State University          |     1|        60|     2.8042|
| Carlsen William          | Pennsylvania State University          |     1|         0|     0.0000|
| Cashon Cara              | University of Louisville               |     1|         0|     0.0000|
| Davidson Natalie         | University of Michigan                 |     1|         0|     0.0000|
| DeJoseph Meriah          | New York University                    |     1|      1480|    65.7253|
| Demuth Katherine         | Macquarie University                   |     1|       630|   302.9233|
| Donovan Andrea Marquardt | University of Wisconsin- Madison       |     1|        16|     3.7600|
| Fawcett Christine        | Uppsala University                     |     1|        58|     0.0375|
| Finegood Eric            | New York University                    |     1|      1480|    65.7253|
| Finke Erinn H.           | Pennsylvania State University          |     1|         0|     0.0000|
| Gordon Drew              | Databrary                              |     1|         8|     0.9942|
| Gordon Peter             | Teachers College - Columbia University |     1|         6|     9.0442|
| Hall Matthew L           | University of Connecticut              |     1|       384|    84.8189|
| Hoffmann Matej           | Italian Institute of Technology        |     1|        30|     0.1156|
| Horst Jessica S.         | University of Sussex                   |     1|         0|     0.0000|
| Johnson Scott            | University of California - Los Angeles |     1|         2|     0.0086|
| Krogh-Jespersen Sheila   | DePaul University                      |     1|         0|     0.0000|
| Kyriakou Lara            | NYU                                    |     1|      1480|    65.7253|
| LoBue Vanessa            | Rutgers University                     |     1|         0|     0.0000|
| Lockman Jeff             | Tulane University                      |     1|        30|     0.1156|
| McNabb Aida              | New York University                    |     1|       114|     3.9578|
| Moty Kelsey              | Lehigh University                      |     1|        68|     8.6200|
| Naigles Letitia          | University of Connecticut              |     1|         2|     0.0147|
| Neiderhiser Jenae        | Penn State University                  |     1|         8|     3.8939|
| Newcombe Nora S.         | Temple University                      |     1|        66|    20.9722|
| Norcia Anthony M.        | Stanford University                    |     1|         4|     0.1850|
| Perez-Edgar Koraly       | Pennsylvania State University          |     1|       644|   152.8783|
| Pintar-Breen Alyssa      | New York University                    |     1|      1480|    65.7253|
| Rhodes Marjorie          | New York University                    |     1|         8|     0.0406|
| Ribner Andrew            | New York University                    |     1|      1480|    65.7253|
| Saffran Jenny            | University of Wisconsin - Madison      |     1|         4|     0.0111|
| Seisler Andrea R.        | Penn State University                  |     1|         8|     0.0022|
| Shai Dana                | IDC Herzliya                           |     1|        30|     2.2986|
| Shapiro Liza             | University of Texas at Austin          |     1|         2|     0.0081|
| Shneidman Laura          | University of Chicago                  |     1|       186|    30.4769|
| Simkovic Matus           | University Cologne                     |     1|         0|     0.0000|
| Stout Wyntre             | Lehigh University                      |     1|        68|     8.6200|
| Vouloumanos Athena       | New York University                    |     1|       410|    44.8231|
| Woolley Jacqueline       | University of Texas                    |     1|       178|    19.5969|
| Wyble Brad               | Pennsylvania State University          |     1|         0|     0.0000|
| Yoshida Hanako           | University of Houston                  |     1|        22|     0.0769|

By Sessions
-----------

``` r
pis.vols.sess.hrs %>%
  select(pi, institution, vols, sessions, hrs) %>%
  arrange(desc(sessions)) %>%
  knitr::kable()
```

| pi                       | institution                            |  vols|  sessions|        hrs|
|:-------------------------|:---------------------------------------|-----:|---------:|----------:|
| Tamis-LeMonda Catherine  | New York University                    |     5|      9835|  1297.1506|
| Adolph Karen             | New York University                    |    38|      8507|  1916.5497|
| Raver Cybele             | New York University                    |     3|      1604|    70.3700|
| Blair Clancy             | New York University                    |     2|      1594|    69.6831|
| DeJoseph Meriah          | New York University                    |     1|      1480|    65.7253|
| Finegood Eric            | New York University                    |     1|      1480|    65.7253|
| Kyriakou Lara            | NYU                                    |     1|      1480|    65.7253|
| Pintar-Breen Alyssa      | New York University                    |     1|      1480|    65.7253|
| Ribner Andrew            | New York University                    |     1|      1480|    65.7253|
| Smith Linda B.           | Indiana University                     |     5|      1116|   114.8075|
| Gilmore Rick O.          | The Pennsylvania State University      |    15|      1100|   112.5792|
| Weisberg Deena Skolnick  | University of Pennsylvania             |    12|      1058|    55.6356|
| Raudies Florian          | Boston University                      |     2|      1010|   110.8944|
| Jayaraman Swapnaa        | Indiana University                     |     3|       994|   110.8747|
| Karasik Lana             | CUNY                                   |     2|       973|   405.8867|
| Perez-Edgar Koraly       | Pennsylvania State University          |     1|       644|   152.8783|
| Demuth Katherine         | Macquarie University                   |     1|       630|   302.9233|
| Frank Michael C.         | Stanford University                    |     8|       454|    80.1053|
| Vouloumanos Athena       | New York University                    |     1|       410|    44.8231|
| Oakes Lisa               | UC Davis                               |     2|       404|     7.3550|
| Hall Matthew L           | University of Connecticut              |     1|       384|    84.8189|
| Baker David              | University of Akron                    |     1|       356|    16.7089|
| Needham Amy              | Vanderbilt University                  |     4|       292|    23.5197|
| Messinger Daniel         | University of Miami                    |     2|       214|    19.7519|
| Vishton Peter            | College of William and Mary            |     7|       214|    13.2639|
| Pfautz Anna              | University of Chicago                  |     2|       186|    30.4769|
| Shneidman Laura          | University of Chicago                  |     1|       186|    30.4769|
| Woodward Amanda          | University of Chicago                  |     4|       186|    30.4769|
| Boerger Elizabeth        | Slippery Rock University               |     1|       178|    19.5969|
| Bunce Louise             | University of Winchester               |     1|       178|    19.5969|
| Woolley Jacqueline       | University of Texas                    |     1|       178|    19.5969|
| Fabricius William        | Arizona State University               |     3|       172|    16.3872|
| Foo Vicky                | New York University                    |     4|       166|    11.3847|
| Jimenez-Robbins Carmen   | None                                   |     3|       150|    17.8336|
| McNabb Aida              | New York University                    |     1|       114|     3.9578|
| Heathcock Jill           | Ohio State University                  |     2|        96|     6.5678|
| Staff None               | Databrary                              |     3|        89|    34.6514|
| Bertenthal Bennett I.    | Indiana University                     |     3|        68|     0.2317|
| Brandone Amanda          | Lehigh University                      |     1|        68|     8.6200|
| Moty Kelsey              | Lehigh University                      |     1|        68|     8.6200|
| Stout Wyntre             | Lehigh University                      |     1|        68|     8.6200|
| Newcombe Nora S.         | Temple University                      |     1|        66|    20.9722|
| Buss Kristin A           | Pennsylvania State University          |     1|        60|     2.8042|
| Fawcett Christine        | Uppsala University                     |     1|        58|     0.0375|
| Franchak John            | None                                   |     2|        58|     0.7714|
| Goldstein Thalia R.      | Pace University                        |     2|        52|    10.3194|
| James Karin              | Indiana University                     |     3|        34|     7.5094|
| Swift Jackie             | Arizona State University               |     2|        34|     3.5067|
| Hoffmann Matej           | Italian Institute of Technology        |     1|        30|     0.1156|
| Lockman Jeff             | Tulane University                      |     1|        30|     0.1156|
| Shai Dana                | IDC Herzliya                           |     1|        30|     2.2986|
| Yoshida Hanako           | University of Houston                  |     1|        22|     0.0769|
| Alibali Martha W.        | University of Wisconsin - Madison      |     2|        16|     3.7600|
| Brand Rebecca            | Villanova University                   |     1|        16|     0.6953|
| DeLoache Judy            | University of Virginia                 |     3|        16|     0.0636|
| Donovan Andrea Marquardt | University of Wisconsin- Madison       |     1|        16|     3.7600|
| Blumberg Mark            | University of Iowa                     |     1|        14|     6.5994|
| Brannon Elizabeth M      | Duke University                        |     3|        12|     0.0928|
| Burdick Jessica          | New York University                    |     1|        10|     0.6869|
| Bergelson Elika          | University of Rochester                |     1|         8|     4.0958|
| Fausey Caitlin M.        | University of Oregon                   |     2|         8|     0.0667|
| Gordon Drew              | Databrary                              |     1|         8|     0.9942|
| Neiderhiser Jenae        | Penn State University                  |     1|         8|     3.8939|
| Professor Suzanne Q.     | Databrary                              |     2|         8|     0.0431|
| Rhodes Marjorie          | New York University                    |     1|         8|     0.0406|
| Seisler Andrea R.        | Penn State University                  |     1|         8|     0.0022|
| Amso Dima                | Brown University                       |     3|         6|     0.0256|
| Gordon Peter             | Teachers College - Columbia University |     1|         6|     9.0442|
| Norcia Anthony M.        | Stanford University                    |     1|         4|     0.1850|
| Saffran Jenny            | University of Wisconsin - Madison      |     1|         4|     0.0111|
| Berthier Neil            | University of Massachusetts Amherst    |     1|         2|     0.0842|
| Johnson Scott            | University of California - Los Angeles |     1|         2|     0.0086|
| Naigles Letitia          | University of Connecticut              |     1|         2|     0.0147|
| Shapiro Liza             | University of Texas at Austin          |     1|         2|     0.0081|
| Waxman Sandy             | Northwestern University                |     2|         2|     0.0950|
| Wilkinson Krista         | Pennsylvania State University          |     2|         2|     0.0014|
| Barr Rachel              | Georgetown University                  |     1|         0|     0.0000|
| Carlsen William          | Pennsylvania State University          |     1|         0|     0.0000|
| Cashon Cara              | University of Louisville               |     1|         0|     0.0000|
| Cordes Sara              | Boston College                         |     2|         0|     0.0000|
| Davidson Natalie         | University of Michigan                 |     1|         0|     0.0000|
| Finke Erinn H.           | Pennsylvania State University          |     1|         0|     0.0000|
| Gelman Susan             | University of Michigan                 |     4|         0|     0.0000|
| Horst Jessica S.         | University of Sussex                   |     1|         0|     0.0000|
| Krogh-Jespersen Sheila   | DePaul University                      |     1|         0|     0.0000|
| LoBue Vanessa            | Rutgers University                     |     1|         0|     0.0000|
| Simkovic Matus           | University Cologne                     |     1|         0|     0.0000|
| Wyble Brad               | Pennsylvania State University          |     1|         0|     0.0000|

By Hours
--------

``` r
pis.vols.sess.hrs %>%
  select(pi, institution, vols, sessions, hrs) %>%
  arrange(desc(hrs)) %>%
  knitr::kable()
```

| pi                       | institution                            |  vols|  sessions|        hrs|
|:-------------------------|:---------------------------------------|-----:|---------:|----------:|
| Adolph Karen             | New York University                    |    38|      8507|  1916.5497|
| Tamis-LeMonda Catherine  | New York University                    |     5|      9835|  1297.1506|
| Karasik Lana             | CUNY                                   |     2|       973|   405.8867|
| Demuth Katherine         | Macquarie University                   |     1|       630|   302.9233|
| Perez-Edgar Koraly       | Pennsylvania State University          |     1|       644|   152.8783|
| Smith Linda B.           | Indiana University                     |     5|      1116|   114.8075|
| Gilmore Rick O.          | The Pennsylvania State University      |    15|      1100|   112.5792|
| Raudies Florian          | Boston University                      |     2|      1010|   110.8944|
| Jayaraman Swapnaa        | Indiana University                     |     3|       994|   110.8747|
| Hall Matthew L           | University of Connecticut              |     1|       384|    84.8189|
| Frank Michael C.         | Stanford University                    |     8|       454|    80.1053|
| Raver Cybele             | New York University                    |     3|      1604|    70.3700|
| Blair Clancy             | New York University                    |     2|      1594|    69.6831|
| DeJoseph Meriah          | New York University                    |     1|      1480|    65.7253|
| Finegood Eric            | New York University                    |     1|      1480|    65.7253|
| Kyriakou Lara            | NYU                                    |     1|      1480|    65.7253|
| Pintar-Breen Alyssa      | New York University                    |     1|      1480|    65.7253|
| Ribner Andrew            | New York University                    |     1|      1480|    65.7253|
| Weisberg Deena Skolnick  | University of Pennsylvania             |    12|      1058|    55.6356|
| Vouloumanos Athena       | New York University                    |     1|       410|    44.8231|
| Staff None               | Databrary                              |     3|        89|    34.6514|
| Pfautz Anna              | University of Chicago                  |     2|       186|    30.4769|
| Shneidman Laura          | University of Chicago                  |     1|       186|    30.4769|
| Woodward Amanda          | University of Chicago                  |     4|       186|    30.4769|
| Needham Amy              | Vanderbilt University                  |     4|       292|    23.5197|
| Newcombe Nora S.         | Temple University                      |     1|        66|    20.9722|
| Messinger Daniel         | University of Miami                    |     2|       214|    19.7519|
| Boerger Elizabeth        | Slippery Rock University               |     1|       178|    19.5969|
| Bunce Louise             | University of Winchester               |     1|       178|    19.5969|
| Woolley Jacqueline       | University of Texas                    |     1|       178|    19.5969|
| Jimenez-Robbins Carmen   | None                                   |     3|       150|    17.8336|
| Baker David              | University of Akron                    |     1|       356|    16.7089|
| Fabricius William        | Arizona State University               |     3|       172|    16.3872|
| Vishton Peter            | College of William and Mary            |     7|       214|    13.2639|
| Foo Vicky                | New York University                    |     4|       166|    11.3847|
| Goldstein Thalia R.      | Pace University                        |     2|        52|    10.3194|
| Gordon Peter             | Teachers College - Columbia University |     1|         6|     9.0442|
| Brandone Amanda          | Lehigh University                      |     1|        68|     8.6200|
| Moty Kelsey              | Lehigh University                      |     1|        68|     8.6200|
| Stout Wyntre             | Lehigh University                      |     1|        68|     8.6200|
| James Karin              | Indiana University                     |     3|        34|     7.5094|
| Oakes Lisa               | UC Davis                               |     2|       404|     7.3550|
| Blumberg Mark            | University of Iowa                     |     1|        14|     6.5994|
| Heathcock Jill           | Ohio State University                  |     2|        96|     6.5678|
| Bergelson Elika          | University of Rochester                |     1|         8|     4.0958|
| McNabb Aida              | New York University                    |     1|       114|     3.9578|
| Neiderhiser Jenae        | Penn State University                  |     1|         8|     3.8939|
| Alibali Martha W.        | University of Wisconsin - Madison      |     2|        16|     3.7600|
| Donovan Andrea Marquardt | University of Wisconsin- Madison       |     1|        16|     3.7600|
| Swift Jackie             | Arizona State University               |     2|        34|     3.5067|
| Buss Kristin A           | Pennsylvania State University          |     1|        60|     2.8042|
| Shai Dana                | IDC Herzliya                           |     1|        30|     2.2986|
| Gordon Drew              | Databrary                              |     1|         8|     0.9942|
| Franchak John            | None                                   |     2|        58|     0.7714|
| Brand Rebecca            | Villanova University                   |     1|        16|     0.6953|
| Burdick Jessica          | New York University                    |     1|        10|     0.6869|
| Bertenthal Bennett I.    | Indiana University                     |     3|        68|     0.2317|
| Norcia Anthony M.        | Stanford University                    |     1|         4|     0.1850|
| Hoffmann Matej           | Italian Institute of Technology        |     1|        30|     0.1156|
| Lockman Jeff             | Tulane University                      |     1|        30|     0.1156|
| Waxman Sandy             | Northwestern University                |     2|         2|     0.0950|
| Brannon Elizabeth M      | Duke University                        |     3|        12|     0.0928|
| Berthier Neil            | University of Massachusetts Amherst    |     1|         2|     0.0842|
| Yoshida Hanako           | University of Houston                  |     1|        22|     0.0769|
| Fausey Caitlin M.        | University of Oregon                   |     2|         8|     0.0667|
| DeLoache Judy            | University of Virginia                 |     3|        16|     0.0636|
| Professor Suzanne Q.     | Databrary                              |     2|         8|     0.0431|
| Rhodes Marjorie          | New York University                    |     1|         8|     0.0406|
| Fawcett Christine        | Uppsala University                     |     1|        58|     0.0375|
| Amso Dima                | Brown University                       |     3|         6|     0.0256|
| Naigles Letitia          | University of Connecticut              |     1|         2|     0.0147|
| Saffran Jenny            | University of Wisconsin - Madison      |     1|         4|     0.0111|
| Johnson Scott            | University of California - Los Angeles |     1|         2|     0.0086|
| Shapiro Liza             | University of Texas at Austin          |     1|         2|     0.0081|
| Seisler Andrea R.        | Penn State University                  |     1|         8|     0.0022|
| Wilkinson Krista         | Pennsylvania State University          |     2|         2|     0.0014|
| Barr Rachel              | Georgetown University                  |     1|         0|     0.0000|
| Carlsen William          | Pennsylvania State University          |     1|         0|     0.0000|
| Cashon Cara              | University of Louisville               |     1|         0|     0.0000|
| Cordes Sara              | Boston College                         |     2|         0|     0.0000|
| Davidson Natalie         | University of Michigan                 |     1|         0|     0.0000|
| Finke Erinn H.           | Pennsylvania State University          |     1|         0|     0.0000|
| Gelman Susan             | University of Michigan                 |     4|         0|     0.0000|
| Horst Jessica S.         | University of Sussex                   |     1|         0|     0.0000|
| Krogh-Jespersen Sheila   | DePaul University                      |     1|         0|     0.0000|
| LoBue Vanessa            | Rutgers University                     |     1|         0|     0.0000|
| Simkovic Matus           | University Cologne                     |     1|         0|     0.0000|
| Wyble Brad               | Pennsylvania State University          |     1|         0|     0.0000|
