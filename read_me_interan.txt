README
======

Thank you for your interest in this data product. Any use of these data should be cited as:

Casey Youngflesh, Jacob Socolar, Bruna R. Amaral, Ali Arab, Robert P. Guralnick, Allen H. Hurlbert, Raphael LaFrance, Stephen J. Mayor, David A. W. Miller, and Morgan W. Tingley. 2021. Migratory strategy drives species-level variation in bird sensitivity to vegetation green-up.


Data used to derive these arrival estimates come from eBird, which should be cited as:

Sullivan, B.L., C.L. Wood, M.J. Iliff, R.E. Bonney, D. Fink, and S. Kelling. 2009. eBird: a citizen-based bird observation network in the biological sciences. Biological Conservation 142: 2282-2292.


Green-up data were derived from the MCD12Q2 (https://lpdaac.usgs.gov/products/mcd12q2v006/) and MCD12Q1 (https://lpdaac.usgs.gov/products/mcd12q1v006/) data products and should be cited as:

Friedl, M., Gray, J., Sulla-Menashe, D. 2019. MCD12Q2 MODIS/Terra+Aqua Land Cover Dynamics Yearly L3 Global 500m SIN Grid V006 [Data set]. NASA EOSDIS Land Processes DAAC.

Friedl, M., Gray, J., Sulla-Menashe, D. 2019. MCD12Q1 MODIS/Terra+Aqua Land Cover Type Yearly L3 Global 500m SIN Grid V006 [Data set]. NASA EOSDIS Land Processes DAAC.


Field Descriptions - data_interan.csv
-------------------------------------
species - species common name
sci_name - species scientific name
year - year
cell - cell number
cell_lat - latitude of cell centroid
cell_lng - longitude of cell centroid
arr_IAR_mean - posterior mean of IAR-derived arrival estimate
arr_IAR_sd - posterior mean of IAR-derived arrival estimate
gr_mn - mean 'midgreen-up' date for all pixels classified as 'good' or 'best' that fall within cell that were classified as forest - green-up dates derived from the MCD12Q2 data product - land cover information derived from MCD12Q1 data product


NOTES
-----
In accordance with the eBird Data Access Terms of Use that states that these terms be supplied with data products derived from eBird, those Terms of Use are as follows (accessed from: https://www.birds.cornell.edu/home/ebird-data-access-terms-of-use/):

THESE TERMS AND CONDITIONS OF USE (the “Terms”) ARE A LEGAL AND BINDING AGREEMENT BETWEEN YOU AND CORNELL UNIVERSITY ON BEHALF OF THE CORNELL LAB OF ORNITHOLOGY (hereinafter “Cornell Lab of Ornithology”) governing your use of eBird data:

    *eBird data are supplied only for applied and basic research and education.
    *eBird data will only be used in a bird finding tool to show sightings from eBird.
    *eBird data may not be used in tools that assist users in bird identification using regional and temporal abundance information.
    *The Cornell Lab of Ornithology will update the eBird data available at any time.
    *The users of eBird data will provide a full and appropriate acknowledgment and citation in any materials or publications derived in part or in whole from the data; relevant citation details are provided with each dataset. For any publication making substantial use of the eBird data, the Cornell Lab of Ornithology welcomes opportunities for commenting on the work prior to publication, for collaboration, and for co-authorship if we contribute substantial intellectual input in a publication. Expressions of interest can be sent to ebird@cornell.edu.
    *Reproduction of any eBird data or any products derived from it, either whole or in part, for commercial purposes is prohibited without prior written permission of the Cornell Lab of Ornithology. For the purposes of these Terms of Use, “commercial purposes” means: a) any use by, on behalf of, or to inform or assist the activities of, a commercial entity (an entity that operates ‘for profit’); or b) use by any non-profit entity for the purposes of revenue generation. If you require permission please contact the Cornell Lab of Ornithology via email to ebird@cornell.edu.
    *The recipient will only use the eBird data provided for the purpose for which it was requested. If subsequent or different use is required the recipient must contact the Cornell Lab of Ornithology again for written approval.
    *The recipient will not pass the original datasets on to any third parties and will direct all such thirds parties’ requests for use of eBird data back to the Cornell Lab of Ornithology.
    *The recipient will not publish or publicly distribute eBird data in their original format, either whole or in part, in any media, including but not limited to on a website, FTP site, CD, memory stick. The recipient should provide a link to the original data source location on the Cornell Lab of Ornithology website where appropriate.
    *The recipient may only pass on datasets derived from the Cornell Lab of Ornithology’s original eBird data (in forms not limited to tabular and graphical representations) if these derived data are supplied with the same Terms of Use.
    *Your use of any eBird data does not constitute endorsement by the Cornell Lab of Ornithology of any derived products, reports, or analyses. The Cornell Lab of Ornithology and eBird logos must not be used on any derived products, reports, or analyses, or supporting materials, without express written permission.
    *All eBird data are provided with additional supporting metadata sufficient to make sensible and informed decisions about data use. The recipient must read all supporting metadata prior to any analysis and agree to abide by any stipulations contained therein. Where appropriate, Cornell Lab of Ornithology staff can provide additional guidance on request to aid in the correct use and interpretation of the data.
    *The Cornell Lab of Ornithology endeavors to maintain accurate and up-to-date data at all times, but can accept no responsibility for the consequences of errors or omissions in the data, for misuse of the data by any organization or individual, or for any damage done to computing systems into which the data are entered (see Disclaimer below).
    *User agrees to send, free of change, an electronic copy of all products published using eBird data supplied by the Cornell Lab of Ornithology to Jenna Curtis at: Cornell Lab of Ornithology, 159 Sapsucker Woods Road, Ithaca, NY, 14850, USA; or via email to eBird@cornell.edu.
