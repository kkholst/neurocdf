\name{renameImageFiles}
\alias{renameImageFiles}
\title{Rename image directories}
\usage{
  renameImageFiles(dir, newnames, ...)
}
\arguments{
  \item{dir}{Path to process}

  \item{newnames}{New names of image-directories}

  \item{...}{Additional arguments to lower level functions}
}
\description{
  Assuming that the files are on the prefered form
  './subject-id/image-types/files' but where 'image-types'
  are for some reason not matched across different
  subject-ids. This function will then rename the
  'image-types' for each 'subject-id' to the 'newnames'.
  The image-type directories in each subject-id is
  lexically sorted before the renaming procedure!
}
\author{
  Klaus K. Holst
}

