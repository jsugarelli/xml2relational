The xml2relational Package
================
Joachim Zuckarelli

## What `xml2relational` does

`xml2relational` is designed to convert XML documents with nested object
hierarchies into a set of R dataframes. These dataframes represent the
different tables in a relational data model and are connected amongst
each other by foreign keys. Essentially, `xml2relational` flattens an
object-oriented data structure into a relational data structure.

Once the relational structure is created (and that is basically a list
of dataframes representing the different tables) you can export both the
data model (as SQL `CREATE` statements) and the data (either as SQL
`INSERT` statements or as CSV files) to get the data easily into a
relational database.

## Getting started

## Installing and loading `xml2relational`

You can install the `xml2relational` package from CRAN by executing the
following code in your script or in the R console:

``` r
install.packages("xml2relational", dependencies = TRUE)
```

After having installed the package you need to load it (attach it to the
search path) by calling `library()`:

``` r
library(xml2relational)
```

### Our example data set

To demonstrate how `xml2relational` works, we will use a small sample
dataset that is shipped together with the `xml2relational` package: the
`customer` dataset.

Here is how it looks like:

    <xml>
        <customer> 
            <customerno>C0023751</customerno>
            <givenname>Sarah</givenname>
            <surname>Durbin</surname>
            <email>sarah.durbin@absolutelynowhere.com</email>
            <address>
                <street>139 W Jackson Blvd</street>
                <postalcode>60604</postalcode>
                <city>
                    <name>Chicago</name>
                    <state>Illinois</state>
                </city>
                <country>
                    <name>United States of America</name>
                    <isocode>US</isocode>
                </country>
            </address>
            <username>queenofqueens</username>
        </customer>
    
        <customer>
            <customerno>C0017439</customerno>
            <givenname>Mark</givenname>
            <surname>Durbin</surname>
            <email>mark@durbinshome.net</email>
            <address>
                <street>139 W Jackson Blvd</street>
                <postalcode>60604</postalcode>
                <city>
                    <name>Chicago</name>
                    <state>Illinois</state>
                </city>
                <country>
                    <name>United States of America</name>
                    <isocode>US</isocode>
                </country>
            </address>
            <username>durby82</username>    
        </customer>
    
        <customer>
            <customerno>C0248538</customerno> 
            <givenname>Max</givenname>
            <surname>Brunner</surname>
            <email>mbrunner@winetasting-brunner.de</email>
            <address>
                <street>Rotkreuzplatz 5</street>
                <postalcode>80634</postalcode>
                <city>
                    <name>Munich</name>
                    <state>Bavaria</state>
                </city>     
                <country>
                    <name>Germany</name>
                    <isocode>DE</isocode>
                </country>
            </address>
            <username>brunnermax_69</username>  
        </customer>
    
        <customer>
            <customerno>C0271182</customerno>
            <givenname>Urs</givenname>
            <surname>Richli</surname>
            <email>urs.richli@richli-design.ch</email>
            <address>
                <street>Seestrasse 43</street>
                <postalcode>6052</postalcode>
                <city>
                    <name>Hergiswil</name>
                    <state>Luzern</state>
                </city>
                <country>
                    <name>Switzerland</name>
                    <isocode>CH</isocode>
                </country>
            </address>
            <username>ursrichli</username>
        </customer>
    
        <customer>
            <customerno>C0019935</customerno> 
            <givenname>Clara-Sophie</givenname>
            <surname>Dr. Hellmann</surname>
            <email>clara-sophie@ginternetpost.de</email>
            <address>
                <street>Brienner Strasse 11</street>
                <postalcode>80333</postalcode>
                <city>
                    <name>Munich</name>
                    <state>Bavaria</state>
                </city>     
                <country>
                    <name>Germany</name>
                    <isocode>DE</isocode>
                </country>
            </address>
            <username>helli</username>  
        </customer>
    
        <customer>
            <customerno>C0019935</customerno> 
            <givenname>Thomas</givenname>
            <surname>Chang</surname>
            <email>chang-thomas@sf-foryou.com</email>
            <address>
                <street>539 Lombard St</street>
                <postalcode>94133</postalcode>
                <city>
                    <name>San Francisco</name>
                    <state>California</state>
                </city>     
                <country>
                    <name>United States of America</name>
                    <isocode>US</isocode>
                </country>
            </address>
            <username>tchango123</username> 
        </customer>
    </xml>

In this dataset we have a nested object structure. Specifically, each
customer has an address consisting of several elements. Among those
elements is the city which is again an object of its own, with a city
name and state. The same applies to the country which is included with
its name and its ISO country code. When you look at the (completely
made-up) customers here, you will notice that the customers Sarah Durbin
and Mark Durbin (the first two customers) share the same address. Also,
Max Brunner and Clara-Sophie Hellmann both live in Munich, Germany
(although at different addresses). Thomas Chang of San Francisco lives
in the USA, as do the Durbins.

When we now process the data and derive the relational data model,
`xml2relational` will take care of these ‘duplicates’.

### Processing the data

Deriving the relational data model from this XML data is fairly simple:

``` r
customer.data <- toRelational("customers.xml")
```

The `toRelational()` function flattens the hierarchical structure of the
XML data and distributes the data to a set of dataframes representing
the tables of our relational data model. It returns these dataframes as
a list (`customer.data`). We can now inspect this list to see the tables
that have been generated:

``` r
class(customer.data)
```

    ## [1] "list"

``` r
names(customer.data)
```

    ## [1] "xml"      "customer" "address"  "city"     "country"

``` r
class(customer.data$customer)
```

    ## [1] "data.frame"

Let us have a closer look at the `customer` dataframe:

``` r
customer.data$customer
```

    ##   ID_customer customerno    givenname      surname
    ## 1      908503   C0023751        Sarah       Durbin
    ## 2      622386   C0017439         Mark       Durbin
    ## 3       54904   C0248538          Max      Brunner
    ## 4      966861   C0271182          Urs       Richli
    ## 5       66802   C0019935 Clara-Sophie Dr. Hellmann
    ## 6      481398   C0019935       Thomas        Chang
    ##                                email FKID_address      username
    ## 1 sarah.durbin@absolutelynowhere.com       580621 queenofqueens
    ## 2               mark@durbinshome.net       580621       durby82
    ## 3    mbrunner@winetasting-brunner.de       410413 brunnermax_69
    ## 4        urs.richli@richli-design.ch       412941     ursrichli
    ## 5      clara-sophie@ginternetpost.de       193389         helli
    ## 6         chang-thomas@sf-foryou.com        30581    tchango123

As you can see, each customer record has been assigned a primary key,
`ID_customer`. The argument `prefix.primary` of the `toRelational()`
function lets you change the prefix that is used to identify primary key
fields. Its default value is `"ID_"`. Similiarly, using the
`prefix.foreign` argument you can change the prefix used for the names
of foreign key fields from its default value `"FKID_"` to whatever you
like. The name of the key fields always consists of the prefix and the
name of the table.

In the `customer` table we have a foreign key that relates to the
address. You may have noticed that, as expected, the data records of
Sarah and Mark Durbin point to the same `address` record as they live in
the same place.

Let us now look into the address table:

``` r
customer.data$address
```

    ##   ID_address              street postalcode FKID_city FKID_country
    ## 1     580621  139 W Jackson Blvd      60604    621329       473079
    ## 2     410413     Rotkreuzplatz 5      80634    757239       185200
    ## 3     412941       Seestrasse 43       6052    406741       939506
    ## 4     193389 Brienner Strasse 11      80333    757239       185200
    ## 5      30581      539 Lombard St      94133    649895       473079

Again, the address points to other tables, namely the `city` and the
`country` table. As we would have expected, the two Munich addresses
point to the same city and the same country, and the two US addresses
point to the same record in the `country` table.

You see how easy it is to flatten a hierarchical, objected-oriented XML
data structure to a relational data model using the `toRelational()`
function.

### Saving the results

In the next step, we want to export our results. That can mean two
things:

  - exporting the data model (i.e. the structure of the tables)
  - exporting the data, the content of the tables.

For the first task, `xml2relational` provides the `getCreateSQL()`
function. This function returns ready-to-excecute SQL `CREATE`
statements. It supports three built-in SQL flavors, `MySQL`,
`TransactSQL` and `Oracle`. You add additional SQL flavors, if you like.
In this case, you would use `sql.style` argument to provide a special
dataframe containing the required definitions for the new SQL dialect.
Please consult the online help texts for more information on how this is
done.

In order to generate proper SQL `CREATE` statements, `getCreateSQL()`
guesses the data types of the table fields from the data. If you do not
like the results, you can provide your own function to derive the data
types as `datatype.func` argument. This function would need to accept
exactly one argument, a vector with the field vales of the field for
which a datatype needs to be guessed. It then must return the datatype
as a one-element character vector.

If you are not going to change the behavior of `getCreateSQL()` using
these options, generating the SQL `CREATE` statements is very
straightforward:

``` r
create.sql <- getCreateSQL(customer.data, "MySQL")
cat(create.sql, sep="\n\n")
```

    ## CREATE TABLE xml (
    ## PRIMARY KEY (ID_xml)
    ## , ID_xml BIGINT
    ## , FOREIGN KEY (FKID_customer) REFERENCES customer(ID_customer)
    ## , FKID_customer BIGINT
    ## );
    ## 
    ## CREATE TABLE customer (
    ## PRIMARY KEY (ID_customer)
    ## , ID_customer BIGINT
    ## , customerno VARCHAR(8) NOT NULL
    ## , givenname VARCHAR(12) NOT NULL
    ## , surname VARCHAR(12) NOT NULL
    ## , email VARCHAR(34) NOT NULL
    ## , FOREIGN KEY (FKID_address) REFERENCES address(ID_address)
    ## , FKID_address BIGINT
    ## , username VARCHAR(13) NOT NULL
    ## );
    ## 
    ## CREATE TABLE address (
    ## PRIMARY KEY (ID_address)
    ## , ID_address BIGINT
    ## , street VARCHAR(19) NOT NULL
    ## , postalcode BIGINT NOT NULL
    ## , FOREIGN KEY (FKID_city) REFERENCES city(ID_city)
    ## , FKID_city BIGINT
    ## , FOREIGN KEY (FKID_country) REFERENCES country(ID_country)
    ## , FKID_country BIGINT
    ## );
    ## 
    ## CREATE TABLE city (
    ## PRIMARY KEY (ID_city)
    ## , ID_city BIGINT
    ## , name VARCHAR(13) NOT NULL
    ## , state VARCHAR(10) NOT NULL
    ## );
    ## 
    ## CREATE TABLE country (
    ## PRIMARY KEY (ID_country)
    ## , ID_country BIGINT
    ## , name VARCHAR(24) NOT NULL
    ## , isocode VARCHAR(2) NOT NULL
    ## );

`xml2relational` tries to guess the datatype from the actual data. When
you are working with the `MySQL`, `Transact SQL` (`T-SQL`) and `Oracle`
dialects/flavors of SQL, this should be alright. Nevertheless, using the
`datatype.func` argument of `getcreateSQL()` you can also provide your
own function to determine the data type. This function would need to
take exactly one argument, a data vector from a data table, and return
the appropriate SQL data type as a one-element character vector.
Alternatively, you can also use the built-in mechanism for determining
the data type and just supply additional information on the SQL flavor
that you use. Please consult the online help with `?getCreateSQL` to
learn more on providing the necessary information.

By setting the logical `one.statement` argument to `TRUE` you can let
`getcreateSQL()` return the `CREATE` statements in one character value
instead of a vector with one element per `CREATE` statement. In this
case you can use the `line.break` argument to define how the different
`CREATE` statement are to be separated (apart from a semicolon that is
added by default).

To export the data as such you habe two options:

  - you export ready-to-execute SQL `INSERT` statements using
    `getInsertSQL()` function
  - you save the data to CSV files using `savetofiles()`.

Producing SQL `INSERT` statements for the data in one of the tables is
very easy with `getInsertSQL()`:

``` r
insert.sql <- getInsertSQL(customer.data, table.name = "city")
cat(insert.sql, sep="\n")
```

    ## INSERT INTO city(ID_city, name, state) VALUES (621329, 'Chicago', 'Illinois');
    ## INSERT INTO city(ID_city, name, state) VALUES (757239, 'Munich', 'Bavaria');
    ## INSERT INTO city(ID_city, name, state) VALUES (406741, 'Hergiswil', 'Luzern');
    ## INSERT INTO city(ID_city, name, state) VALUES (649895, 'San Francisco', 'California');

You can also export all the tables of your relational model with
`savetofiles()`:

``` r
savetofiles(customer.data)
```

This will save as many CSV files to your current working directory as
you have tables in you model (`customer.data`). Each file is named for
the name of the dataframe connected to the respective table, so
`city.csv` will store the data from the `city` table.

More optional arguments for most of the functions discussed here are
available. Please check the online help for more details.

## Contact the author

I appreciate your questions, issues and feature request. Contact me on
<joachim@zuckarelli.de>, visit the GitHub repository on
<https://github.com/jsugarelli/xml2relational> for the packages source
and [follow me on Twitter](https://twitter.com/jsugarelli) to stay
up-to-date\!
