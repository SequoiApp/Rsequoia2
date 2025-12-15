# get_parca_etalab() works with real API (local only)

    Code
      str(sf::st_drop_geometry(out))
    Output
      tibble [1 x 10] (S3: tbl_df/tbl/data.frame)
       $ idu       : chr "33103000AB0060"
       $ commune   : chr "33103"
       $ prefixe   : chr "000"
       $ section   : chr "AB"
       $ numero    : chr "0060"
       $ contenance: int 150
       $ arpente   : logi FALSE
       $ created   : Date[1:1], format: "2011-05-06"
       $ updated   : Date[1:1], format: "2014-04-29"
       $ SOURCE    : chr "etalab"

