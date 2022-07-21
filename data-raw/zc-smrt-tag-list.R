## code to prepare `zc_smrt_tag_list` dataset goes here

zc_smrt_tag_list <- data.frame(tag_id = c('Zica-20180113-173188',
                                          'Zica-20180330-173188',
                                          'Zica-20180331-173187',
                                          'Zica-20190111-173186',
                                          'Zica-20190113-151361',
                                          'Zica-20191012-144029',
                                          'Zica-20191012-145101',
                                          'Zica-20191111-94810',
                                          'Zica-20191117-195993',
                                          'Zica-20200106-195994', # SHORT one
                                          'Zica-20220112-195994',
                                          'Zica-20211113-195993',
                                          'Zica-20211112-94819'
                                          ))

usethis::use_data(zc_smrt_tag_list, overwrite = TRUE)
