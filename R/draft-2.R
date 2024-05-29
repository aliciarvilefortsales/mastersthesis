## Duração de intervalo

tz <- "America/Sao_Paulo"

start <- lubridate::dmy_hms("15/01/2023 09:57:35", tz = tz)
end <- lubridate::dmy_hms("18/01/2023 23:59:35", tz = tz)
# start <- lubridate::ymd_hms("2022-12-10 16:02:35", tz = tz)
# end <- lubridate::ymd_hms("2023-01-18 23:59:35", tz = tz)

lubridate::interval(start, end, tzone = tz) |> lubridate::as.period()

## Cronotipo

wd <- 6 # Número total de dias de trabalho
fd <- 17 # Número total de dias livres
# FPS nos dias de trabalho
sd_w <- lubridate::as.duration(hms::parse_hms("07:01:10"))
so_f <- hms::parse_hms("01:55:42") # HIS nos dias livres
# FPS nos dias livres
sd_f <- lubridate::as.duration(hms::parse_hms("07:24:31"))
# FPS média ponderada
sd_mean <- ((sd_w * wd) + (sd_f * fd)) / (wd + fd)
msf <- mctq::msl(so_f, sd_f)
msf_sc <- mctq::msf_sc(msf, sd_w, sd_f, sd_mean, FALSE)

# Meio-sono nos dias livres corrigido (proxy do cronotipo)
lubritime::round_time(msf_sc)
