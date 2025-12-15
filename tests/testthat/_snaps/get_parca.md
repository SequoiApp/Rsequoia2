# get_parca() works (local only)

    Code
      str(sf::st_drop_geometry(out))
    Output
      'data.frame':	1 obs. of  13 variables:
       $ IDU     : chr "33103000AB0060"
       $ REG_NOM : chr "NOUVELLE AQUITAINE"
       $ REG_CODE: chr "75"
       $ DEP_NOM : chr "GIRONDE"
       $ DEP_NUM : chr "33"
       $ COM_NOM : chr "CASTELMORON D ALBRET"
       $ COM_CODE: chr "33103"
       $ PREFIXE : chr "000"
       $ SECTION : chr "AB"
       $ NUMERO  : chr "0060"
       $ LIEU_DIT: chr NA
       $ SURF_CA : num 150
       $ SOURCE  : chr "etalab"

---

    Code
      str(sf::st_drop_geometry(out))
    Output
      'data.frame':	1 obs. of  13 variables:
       $ IDU     : chr "33103000AB0060"
       $ REG_NOM : chr "NOUVELLE AQUITAINE"
       $ REG_CODE: chr "75"
       $ DEP_NOM : chr "GIRONDE"
       $ DEP_NUM : chr "33"
       $ COM_NOM : chr "CASTELMORON D ALBRET"
       $ COM_CODE: chr "33103"
       $ PREFIXE : chr "000"
       $ SECTION : chr "AB"
       $ NUMERO  : chr "0060"
       $ LIEU_DIT: chr "LE BOURG"
       $ SURF_CA : num 150
       $ SOURCE  : chr "etalab"

---

    Code
      str(sf::st_drop_geometry(out))
    Output
      'data.frame':	1 obs. of  13 variables:
       $ IDU     : chr "33103000AB0060"
       $ REG_NOM : chr "NOUVELLE AQUITAINE"
       $ REG_CODE: chr "75"
       $ DEP_NOM : chr "GIRONDE"
       $ DEP_NUM : chr "33"
       $ COM_NOM : chr "CASTELMORON D ALBRET"
       $ COM_CODE: chr "33103"
       $ PREFIXE : chr "000"
       $ SECTION : chr "AB"
       $ NUMERO  : chr "0060"
       $ LIEU_DIT: chr NA
       $ SURF_CA : num 150
       $ SOURCE  : chr "etalab"

