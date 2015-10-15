#' Encode PNG to base64 and provide meta data for JSON payload
#'
#'
#' @param image character vector of unencoded PNG file location
#'
#' @return data frame with image type, image name (which matches CID in Mandrill template), and
#' base 64 encoded image.
process_images <- function(image){
  # create tempfile
  temp <- tempfile()

  #get image name
  image_name <- gsub("(.+\\/)(.+$)",
                     "\\2",
                     image,
                     perl = T) %>%
    stringr::str_replace("(//|/)", "") %>%
    stringr::str_replace(".png", "")

  #enocde png as base64
  base64::encode(image, temp) # called for side effect.  saves encoded image in temp

  # get encoded image
  image_encoded <- paste(readLines(temp), collapse = "")

  #unlink temp file
  unlink(temp)

  out_df <- data.frame(type = "img/png",
                     name = image_name,
                     content = image_encoded,
                     stringsAsFactors = FALSE
                     )

  # return
  out_df
}

#' Process HTML contents passed to `mc:edit` classes in Mandrill template
#'
#' @param character vector of HTML to replace contents btween HTML tags in Mandrill Template
#'
#' @return data frame with content name (matching `mc:edit="content_name"` in template)
#'
process_contents <- function(content){
  content_name <- deparse(substitute(content))
  out_df <- data.frame(name = content_name,
                     content = content[[1]],
                     stringsAsFactors = FALSE)

  # return
  out_df
}

#' Encode attachment to base64 and provide meta data for JSON payload
#'
#'
#' @param attachment character vector of unencoded attachment file location
#'
#' @return data frame with attachement type, attachment name (which matches CID in Mandrill template), and
#' base 64 encoded attachement.
process_attachments <- function(attachment){


  #expand path (needed if tilde is in name)
  attachment <- path.expand(attachment)

  # create tempfile
  temp <- tempfile()

  #get image name
  attachment_name <- gsub("(.+\\/)(.+$)",
                     "\\2",
                     attachment,
                     perl = T) %>%
    stringr::str_replace("(//|/)", "")

  #enocde attachment as base64
  base64::encode(attachment, temp) # called for side effect.  saves encoded attachment in temp


  # get encoded image
  attachment_encoded <- paste(readLines(temp), collapse = "")

  # unlink temp file
  unlink(temp)

  #get attachement mime-type
  attachment_mime_type <- mime::guess_type(attachment)

  out_df <- data.frame(type = attachment_mime_type,
                       name = attachment_name,
                       content = attachment_encoded,
                       stringsAsFactors = FALSE
  )

  # return
  out_df
}
