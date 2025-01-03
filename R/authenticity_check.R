#' @importFrom digest digest
#' @name authenticity_check
#' @aliases authenticity_check
#' @title Check for authenticity of the analysis author or tutor from an indexed database
#' @description
#' Authenticity check for institutional analysis. Future implementation of this code is planned for incoming functions.
#'
#'
#' @param name Initials of the code's author name. (Last name, first name)
#' @param affil Affiliation of the code's author
#'
#'
#'
#' @examples
#' \dontrun{ #For John Doe
#' authenticity_check(name = JD, affil = 'Some University')}
#' authenticity_check(name = 'FGJA', affil = 'UASLP')
#'
#' @return Text in console whether the statistical analysis is authorized by a tutor or not
#'
#' @export


authenticity_check<-function(name, affil){
  namehash<- digest(name, algo = "sha512", serialize = FALSE)

  auth1<-"ab0afaf7ca51bd716836a7ead4abeae5f20abaa6f4b40aa847075d92b4bae62e33d18f974263eec7f56634a8a49adc92ddef1ff6d85449fcf25c9b353e216cdd"
  auth2<-"2ac9a6f0db594ee1b8036eff41d8b733bb56594d14421840475a155ee875be71ddaa40bf570b1a277434f93be9647c4c45502208511fedef16f94f8a75289d3a"

  auth1ua<-"9f60a952e8ba02c49c70b99416af983076638b64ac8eaa693e8a251c8ebca9c54808f711f42e52b016ae78579d9aa2969afda3786f8e111b9733cc758c434de8"
  auth2ua<-"bba39d044625b04a70b0879c896a572728c252ad9558c137b10152114f092c2468976f21c7fd52fc1319953e75d5e4d2cd316b23d3cb8b3f1495e85a60ecf3f2"
  auth3ua<-"14f88da5c0273b7b7d041d32b298f598f2a92b72f80f5e5ef1c000f712348bf4ab75e1cee214e88952aa1895a6c3fc3459e3ebeb8795d7e5185ea90dc626a661"
  auth4ua<-"b5a56524823802ec38c29f1fc8bcac70917175b75897d0fb7cbd84e508e3b4977cbd4ab467b94ea9d7daa7504c2128badcc9383d3839b48b90736cee21573674"

  if(affil == "SC"){
    if(namehash == auth1 | namehash == auth2){
      cat("Statistical analysis is authentic. \u2660 \n")
    } else {
      cat("Statistical analysis is not authentic. \n", "\n","Please check with your tutor or committee. \n")
    }
  } else {
  if(affil == "UASLP"){
    if(namehash == auth1ua | namehash == auth2ua | namehash == auth3ua | namehash ==  auth4ua){

      cat("Statistical analysis is authentic. \n")
    } else {
      cat("Statistical analysis is not authentic. \n", "\n","Please check with your tutor or committee.\n")
    }
  }
  }
}

#' @title Author checker
#' @name author_check
#' @importFrom stringi stri_detect_fixed
#'
#' @param name Initials of the code's author name. (Last name, first name)
#' @param credentials Dataframe with the hash code of the tutor. Can be loaded via 'get_credentials' function.
#' @param hash_col Name of the column with hash codes. Default is "HASH"
#'
#' @description
#' Function to check if the name of an author is in a database of validated credentials. Credentials in a database must be stored as HASH. Hash generated by the function uses the following arguments: digest(text, algo= 'sha512', serialize=F). The function is planned to be used by future code implementations.
#'
#' @return Text in console whether the statistical analysis is authorized by a tutor or not
#'
#' @examples
#' \dontrun{
#'author_check("FGA", credentials, hash_col = "HASH")}
#'
#'
#' @export

author_check<-function(name, credentials, hash_col = "HASH"){

  namehash<- digest(name, algo = "sha512", serialize = FALSE)

    if(is.data.frame(credentials)){
      if(any(stringi::stri_detect_fixed(credentials[[hash_col]], pattern = namehash))){

        cat("Statistical analysis is authentic. \n")
      } else {
        cat("Statistical analysis is not authentic. \n", "\n","Please check with your tutor or committee.\n")
      }
    } else {
      cat("Affiliation data must be a dataframe")
    }
}

#' @title Get credentials for author check
#' @name get_credentials
#' @param data Name of the file which the credentials are stored.
#' @param sep Separator of the dataframe. Default is "," to denote a .csv file.
#'
#' @return A dataframe with authorization credentials
#'
#' @examples
#' \dontrun{
#' credentials<-get_credentials(data = "googlesheets_url", sep=",")}
#'

get_credentials<-function(data=NULL, sep=","){
  if(is.null(data)){
    stop("Credentials database should be a dataframe")
  }
  credentials<<-read.delim(data, sep=sep)
  assign("credentials", credentials)
}


