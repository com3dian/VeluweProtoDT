# VeluweProtoDT
> Veluwe proto-DT: a digital mini twin of tree phenology and climate scenarios

<details>
<summary><h3>Table of Contents</h3></summary>

  - [Project info](#project-info)
    - [Description](#description)  
    - [Team members](#team-members)
  - [Style guide](#style-guide)
    - [Code metadata](#code-metadata)
    - [Code annotation](#annotation) 
    - [Naming convention](#naming) 
    - [Syntax](#syntax)
    - [Qualifying namespaces](#namespace)
</details>

## Project info <a name="project-info"/>

### Description <a name="description"/>
The goal of this project is to provide a full but simplified workflow that resembles one potential LTER-LIFE user experience. Consider it the naieve ecologists' view of a digital twin. The subject of this proto-DT is the timing of bud burst, derived from NIOO's bud burst dataset (i.e., the first dataset FAIRified as part of the [FDFDT](https://github.com/LTER-LIFE/FDFDT) pilot project, a collaboration between DANS and NIOO), in relation to historic and future temperature scenarios. The workflow is developed using the Notebook-as-a-Virtual-Research-Environment (NaaVRE) framework, built by LifeWatch; see [https://github.com/QCDIS/NaaVRE](https://github.com/QCDIS/NaaVRE).

Find associated GitHub project here: [Veluwe proto-DT](https://github.com/orgs/LTER-LIFE/projects/3)

### Team members <a name="team-members"/>
- [Cherine Jantzen](https://github.com/CherineJ) / NIOO-KNAW (ORCID: [0009-0006-0723-2682](https://orcid.org/0009-0006-0723-2682))
- [Stefan Vriend](https://github.com/StefanVriend) / NIOO-KNAW (ORCID: [0000-0002-9006-5988](http://orcid.org/0000-0002-9006-5988))
- Marcel Visser / NIOO-KNAW (ORCID: [0000-0002-1456-1939](http://orcid.org/0000-0002-1456-1939))
- [Gabriel Pelouze](https://github.com/gpelouze) / LifeWatch ERIC / University of Amsterdam
- [Spiros Koulouzis](https://github.com/skoulouzis) / LifeWatch ERIC / University of Amsterdam

## Style guide <a name="style-guide">
The following guide describes the programming style that we use throughout this project. The goal of this guide is to make our code easier to read, share, and verify. Heavily influenced by the [tidyverse style guide](https://style.tidyverse.org/index.html).

### Code organisation <a name="code-organisation"/>

#### Code metadata 
Every script should start with a metadata header including at least title, author, date of creation, and date of last update.
```r
# Title: Create Style Guide
# Author: Stefan Vriend
# Created: 2023/12/14
# Last updated: 2023/12/15
```
#### Packages
Load all used packages at the top of the script, after the code metadata.

### Code annotation <a name="annotation"/>
Comment your code. Each line of a comment should begin with the comment symbol and a single space: `#`. Comments should explain the why, not the what.
Use section labels (Ctrl + Shift + R in Rstudio) to divide your code in sections.

### Naming convention  <a name="naming"/>

#### File names
Names of files should all be meaningful. If files need to be run in order, prefix them with numbers (e.g., "0_retrieve-data.R", "1_process-data.R"). If you later realise that you’ve missed some steps, it’s tempting to use "2a", "2b", etc. However, it’s generally better to bite the bullet and rename all files.

#### Object and variable names
Use underscore in object and variables names to separate words. Generally, variable names should be nouns and function names should be verbs. Strive for names that are concise and meaningful.
Where possible, avoid using names of existing functions and variables. Doing so will cause confusion for the readers of your code.

### Syntax <a name="syntax"/>

#### Named arguments
A function’s arguments typically fall into two broad categories: one supplies the data to compute on; the other controls the details of computation. When you call a function, you may omit the names of data arguments, because they are used so commonly. If you override the default value of an argument, use the full name.
<details>
  <summary><i>Example</i></summary>

  ```r
  # Preferred
  mean(1:10, na.rm = TRUE)
  mean(x = 1:10, na.rm = TRUE)

  # Not preferred
  mean(x = 1:10, , FALSE)
  ```
</details>


#### Spacing
Place spaces around all infix operators (`=`, `+`, `-`, `<-`, etc.). The same rule applies when using `=` in function calls. Always put a space after a comma, and never before (just like in regular English).
<details>
  <summary><i>Example</i></summary>

  ```r
  average <- mean(length + width / 2, na.rm = TRUE)
  ```
</details>

Place a space before left parentheses, except in a function call.
<details>
  <summary><i>Example</i></summary>
  
  ```r
  if (debug == TRUE) do(x)
  plot(x, y)
  ```
</details>

#### Curly braces
An opening curly brace should never go on its own line and should always be followed by a new line. A closing curly brace should always go on its own line, unless it’s followed by else. Always indent the code inside curly braces.
<details>
  <summary><i>Example</i></summary>
  
  ```r
  if (y == 0) {
    log(x)
  } else {
    y ^ x
  }
  ```
</details>

#### Long lines
Try to limit your code to approximately 80 characters per line. This fits comfortably on a printed page with a reasonably sized font. 
If you find yourself running out of room, move to next line.

#### Quoting text
Try to stick to one quotation mark throughout the script. Preferred `" "`.

#### Tidyverse quotations
Use quotation marks around variable names when using functions that _select_ variables, such as `dplyr::select()`, `dplyr::rename()`, or `tidyr::pivot_longer()`.
It is not necessary to use quotation marks around variable names when using functions that _filter_ or _change_ variables, such as `dplyr::filter()`, `dplyr::mutate()`, `dplyr::summarise()`.

#### Logical
Preferred `TRUE` and `FALSE` over `T` and `F`.

### Qualifying namespaces <a name="namespace"/>
Users should explicitly qualify namespaces for all external functions.
<details>
  <summary><i>Example</i></summary>
  
  ``` r
  dplyr::mutate()
  ```
</details>
