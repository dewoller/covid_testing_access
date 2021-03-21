#source('_drake.R')


get_lga_covid_cases = function() {

  list.files('data/cases', full.names=TRUE) %>%
    enframe(name=NULL, value='file') %>%
    mutate( cases = map(file, function(file) {
                          read_html(  file)  %>%
                            html_table() %>%
                            pluck(1) %>%
                            as_tibble() %>%
                            clean_names() %>%
                            set_names(c('lga','confirmed','active')) %>%
                            mutate(confirmed = as.character(confirmed)) %>%
                            mutate(active = as.character(active)) 
})) %>%
    mutate( as_of_date = str_replace(file, 'data/cases/coronavirus-update-victoria-','' ) ) %>%
    mutate( as_of_date = str_replace(as_of_date, 'july','July' ) ) %>%
    mutate( as_of_date = dmy(as_of_date) ) %>%
    unnest(cases) %>%
    select(-file) %>%
    write_csv('output/covid_cases.csv')


}
