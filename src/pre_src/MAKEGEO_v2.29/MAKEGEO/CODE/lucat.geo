c-----------------------------------------------------------------------
c --- COMMON BLOCK /LUCAT/ -- Info about land use categories     MAKEGEO
c-----------------------------------------------------------------------
      integer outcat(mxocat)

      common /LUCAT/ noutcat,iwat1,iwat2,outcat,nincat,numwat,
     &               imiss,imiss2,cfract,nsplit,
     &               incat(mxcat),z0lu(mxcat),alblu(mxcat),bowlu(mxcat),
     &               soillu(mxcat),qflu(mxcat),xlailu(mxcat),
     &               mapcat(mxcat),iwat(mxcat),
     &               iredef(mxcat),nnrec(mxcat),nrec(mxcat),
     &               irec(mxcat,mxcat),prec(mxcat,mxcat)

c-----------------------------------------------------------------------
c     DEFINITIONS  [i]=integer   [r]=real   [l]=logical   [c]=character
c-----------------------------------------------------------------------
c noutcat          number of output land use categories              [i]
c iwat1,iwat2      range of output land use categories assigned to   [i]
c                  water (inclusive)
c outcat           output land use categories                       [ia]
c nincat           number of input land use categories               [i]
c numwat           number of input water categories                  [i]
c imiss            land use category assigned to cell when no land   [i]
c                  use data are found                
c imiss2           index of 'imiss' category in the 'incat' array    [i]
c cfract           minimum fraction of cell covered by water required[r]
c                  to define the dominant land use as water
c nsplit           number of input land use categories than are      [i]
c                  redefined (split)
c incat            input land use categories                        [ia]
c z0lu             surface roughness (m) for each input category    [ra]
c alblu            surface albedo (0 to 1) for each input category  [ra]
c bowlu            Bowen ratio for each input category              [ra]
c soillu           soil heat flux parameter for each input category [ra]
c qflu             anthropogenic heat flux (W/m**2) for each input  [ra]
c                  category
c xlailu           leaf area index for each input category          [ra]
c mapcat           output category for each input land use category [ia]
c iwat             input categories defined as water                [ia]
c iredef           pointer to the nsplit input land use categories  [ia]
c                  that are redefined; 0=NOT redefined
c                  (nincat values)
c nnrec            index of each input land use category redefined  [ia]
c                  (nsplit values)
c nrec             number of input land use categories that receive [ia]
c                  a portion of each redefined land use
c                  (nsplit values)
c irec             index of each receiving input land use category  [ia]
c prec             percent of each redefined category that is placed[ia]
c                  in each receiving input land use category  
c-----------------------------------------------------------------------
