#' Manejo de contraseñas
#'
#' Funciones para guardar, obtener y eliminar contraseñas de CMIP6
#'
#' @param user,password usuario y contraseña. Si son `NULL`, se piden interactivamente (recomendado).
#'
#' @details
#' El método recomendado es dejar los valores como `NULL`. De esta manera, los datos de
#' autenticación no quedan guardados en texto plano en la historia de comandos de R
#'
#' @name cmip_key
#' @aliases cmip_key_remove cmip_key_get cmip_key_set
NULL


#' @describeIn cmip_key Guarda una cambinación de usuario/contraseña
#' @export
cmip_key_set <- function(user = NULL, password = NULL) {
  if (is.null(user) || is.null(password)) {
    user <- readline("User: ")
    if (is.null(user) | user == "") {
      stop("No user supplied.")
    }
    password <- getPass::getPass(msg = "Password: ", noblank = TRUE)
    if (is.null(password)) {
      stop("No password supplied.")
    }
  }
  if(keyring::default_backend()$name != "env") {
    keyring::keyring_unlock()
  }

  keyring::key_set_with_value(service = .cmip_service(),
                              username = user,
                              password = password)
  return(invisible(user))
}


#' @describeIn cmip_key Lee el password de un usuario.
#' @export
cmip_key_get <- function(user) {
  if(keyring::default_backend()$name != "env") {
    keyring::keyring_unlock()
  }
  keyring::key_get(service = .cmip_service(),
                   username = user)
}


cmip_key_remove <- function(user) {
  keyring::key_delete(service = .cmip_service(),
                      username = user)
}

.cmip_service <- function() {
  "cimadata_cmip"
}



#' Usuario por default
#'
#' @param user usuario para setear como default (`NULL` signfica que no hay usuario por default)
#'
#' @return
#' `cmip_default_user_set()` define el usuario por defecto (para la sesión actual)
#' y devuelve el usuario de forma invisible.
#'
#' `cmip_default_user_get()` devuelve el usuario por default seteado con
#' `cmip_default_user_get()` o el usaurio guardad con `cmip_key_set()` en caso de
#' haber sólo uno.
#'
#' @aliases cmip_default_user_get cmip_default_user_set
#' @name cmip_default_user
NULL

#' @describeIn cmip_default_user Define el usuario por default.
#' @export
cmip_default_user_set <- function(user = NULL) {
  options("CMIP.DEFAULT.USER" = user)
  return(invisible(user))
}

#' @describeIn cmip_default_user Obtiene el usuario por default.
#' @export
cmip_default_user_get <- function() {
  # First priotiy: global option
  user <- getOption("CMIP.DEFAULT.USER", default = NULL)

  if (is.null(user)) {
    # Second priority: check if only one user
    all_keys <- keyring::key_list()
    cmip_keys <- all_keys[all_keys[["service"]] == .cmip_service(), ]

    if (nrow(cmip_keys) == 0) {
      warning("No users saved. Create an account at ", .cmip_url(),
              " and (optionally) use 'cmip_key_set()' to save it.")
      user <- NULL
    } else if (nrow(cmip_keys) > 1) {
      stop("Multiple users present in keyring. Use 'cmip_default_user_set()' ",
           "to set the default user manually.")
    } else {
      user <- cmip_keys[["username"]]
    }
  }

  return(user)
}

.cmip_url <- function() {
  "https://esgf-node.llnl.gov/user/add/"
}

