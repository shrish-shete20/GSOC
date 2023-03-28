F_to_C <- function(F_temp){
    C_temp <- (F_temp - 32) * 5/9;
    cat(gettext("Temperature has been converted from F to C.\n"));
    return(C_temp);
}

C_to_F <- function(C_temp){
    F_temp <- (C_temp * 9/5) + 32;
    cat(gettext("Temperature has been converted from C to F.\n"));
    return(F_temp);
}
