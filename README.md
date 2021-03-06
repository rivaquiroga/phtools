
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phtools

<!-- badges: start -->

![status](https://img.shields.io/badge/status-WIP-lightgrey)
<!-- badges: end -->

The goal of **phtools** is to automate some of the tasks of Programming
Historian editorial team. The aim is to give us more time to be awesome
digital humanities editors, and to avoid, when possible, frictions with
our infrastructure.

## Installation

You can install **phtools** directly from GitHub. You need to have the
remotes package previously installed:

``` r
# install.packages("remotes")
remotes::install_github("rivaquiroga/phtools")
```

## Usage

💡 The whole process will be easier if you set your working directory to
the folder with the files of the lesson you are editing.💡

#### … but what if I prefer to use the command line?

That’s ok. You don’t really need to open R or RStudio to use **phtools**
functions (although, using RStudio or any other IDE has the advantage of
letting you edit the markdown file there, if needed).

To use the functions from the command line, you need to run `Rscript -e`
and the complete name of the function (i.e., including the package name)
inside simple quotation marks (double ones are used in the R code):
`'phtools::the_function_you_want_to_call()'`. For example, if you want
to check the lesson urls, you need to call the `check_links()` function
like this:

``` bash
Rscript -e 'phtools::check_links("awesome-new-lesson.md")'
```

Now let’s take a look at what we can do with **phtools**.

### Checking links

Broken links. What a source of despair. Most of the times broken links
are not in the lesson you are trying to publish, but it is a good idea
to check them before opening the pull request. Just in case. The
`check_links()` function will retrieve the urls found in a lesson file
and give you their current status. **Be patience. This function is not
very fast** ⏳

`check_links()` takes as an argument the markdown file of the lesson you
want to publish (your local version or the one from GitHub). You need to
provide the full path if the file is not in your working directory.

``` r
check_links("awesome-new-lesson.md")

# or

check_links("https://raw.githubusercontent.com/programminghistorian/jekyll/gh-pages/es/lecciones/instalacion-linux.md")
```

If you are lucky, you will get only `Success: (200) OK` 🎉:

    #>                                           url            status
    #> 1          https://es.wikipedia.org/wiki/Sudo Success: (200) OK
    #> 2 https://wiki.python.org/moin/PythonEditors/ Success: (200) OK
    #> 3           http://komodoide.com/komodo-edit/ Success: (200) OK

Errors like not being able to resolve host, will prompt a message like
this one:

    [1] "Could not resolve host: nonexistentwebsite.com"

In those cases, please check if there are no typos in the url, if the
site is really down, or does not exist. The function won’t show you the
rest of the results until you fix those bigger issues 😬. Sorry, I don’t
make the rules.

What if the links redirect to 404 pages? You will see something the
message `Client error: (404) Not Found`. In those cases you need to
check those links to see what is going on. Please take into account that
sometimes

    #>                                        url                        status
    #> 1 https://github.com/programminghistorian/             Success: (200) OK
    #> 2  https://github.com/programinghistorian/ Client error: (404) Not Found

Things can always fails in new, surprising and strange ways. If a
different error appears while checking a lesson, please open an issue in
this repository.

> TODO: check that links to PH are relative links

### Dealing with images

> IMPORTANT: currently, these functions only work for original lessons.
> I’ll open a ticket on our jekyll repo to discuss common criteria
> regarding translated images (and about updating image filenames in
> older lessons).

**Before start**: To get the expected results, you need to check that at
least the .md file is using the slug you want the lesson to have. Images
will be named following that pattern. If the folder with the images is
named differently, don’t worry; there is a function to fix that problem.

According to our [author
guidelines](https://programminghistorian.org/en/author-guidelines#figures-and-images),
images should be named using the lesson slug (e.g., `lesson-slug1.png`,
`lesson-slug2.png`, `lesson-slug3.png`, etc.). To check if this pattern
is used in the lesson you are editing, run the `check_images()`
function. The first argument is the lesson file (with the .md
extension), and the second the folder with the images. If the files are
not in your working directory, you need to provide the full path.

``` r
check_images("awesome-new-lesson.md", "awesome-new-lesson")
```

The function will first check if the images in the lesson and in the
folder match (i.e., they have the same names and there are no extra
images). If you are lucky and everything is alright, you will see a
message like this one:

``` r
ℹ CHECK RESULTS:
✔ Images filenames match folder and lesson.
✔ The images are using the same slug as the lesson file.
✓ You are ready with this part of the process. Excelente :)
```

This means that, regarding image filenaming, you are ready 🎉. A
motivational message in one of the project languages will appear to
cheer you up.

But things usually don’t work correctly the first time. Maybe, there is
a mismatch. For example, the lesson file points to an image file that is
not in the folder, or the folder contains images that the lesson seems
not to be using:

``` r
ℹ CHECK RESULTS:
x Images names in the lesson file does not match the filenames in image folder
x There are images in the folder that are not included in the lesson: awesome-new-lesson-table.jpg
x The lesson is using images that were not found in the folder: awesome-new-lesson2.jpg
• Please solve this issues before runing again `check_images()`
```

If that is the case, you need to check the lesson, fix the mismatch, and
run `check_images()` again.

If images are not named according PH guidelines (lesson-slug1,
lesson-slug2, etc.), you will see a message like this one:

``` r
ℹ CHECK RESULTS:
✓ Images filenames match folder and lesson.
x Images don't have the same slug as the lesson file. You can fix this problem by runing `phtools::rename_images()` 
ℹ FYI, these are the current filenames: 
  loading-files.png
  first-results.png
  possible-error.png
  plotting.png
```

If that is the case, you just need to follow the suggestion of running
`rename_images()` (once you solve the mismatch, if any).

The `check_images()` function will also check the file extensions. `png`
is the recommended format, although `jpg` is also accepted. If jpg files
are found, you will see this message:

``` r
ℹ CHECK RESULTS:
✓ Images filenames match folder and lesson.
✓ The images are using the same slug as the lesson file.
✓ You are ready with this part of the process. Woot!
ℹ (There are images with .jpg or .jpeg extensions. That is ok, but .png is better.)
```

If other formats are found, an error will appear:

``` r
ℹ CHECK RESULTS:
✓ Images filenames match folder and lesson.
✓ The images are using the same slug as the lesson file.
x At least one image in the folder is not using .png or .jpg format, which are the ones required: awesome-new-lesson4.tiff
```

If you don’t want to check the file extensions, you can use de
`check_extensions = FALSE` argument. But I don’t think that is a good
idea, unless you have a very good reason.

If you are done with the checks and the only need you need to solve is
renaming images accoding to the guidelines, just run `rename_images()`
using the same arguments as before:

``` r
rename_images("awesome-new-lesson.md", "awesome-new-lesson")
```

If the folder with the images is not named as the .md file, you will be
ask if you want to change the name. By default, a backup copy of both
lesson and folder will be created. This is good idea, and that is why is
the default behaviour. But if you do not want backup copies, just add
the argument: `backup_files = FALSE`.

### Browsing the editorial guidelines

You can open any of our editorial guidelines using one of the
`open_*_guidelines()` functions + the language code your are interested
in:

``` r
open_author_guidelines("en")
open_editor_guidelines("es")
open_reviewer_guidelines("fr")
open_translator_guidelines("pt")
```

### Soon: checking image captions

I’m looking at you, not escaped quotation mark.

### Soon: yaml validator

Oh, the doi! A very initial version of a Python script to achieve this
goal can be found in [this
repository](https://github.com/rivaquiroga/phyaml).
