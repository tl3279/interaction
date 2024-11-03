writing fxns
================
Tianqi Li
2024-11-03

# My first function

``` r
x_vec = rnorm(25, mean = 5, sd = 3)
(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1] -0.6679852 -0.6719874 -0.5788603  0.4292183 -0.1179703  1.5682129
    ##  [7]  0.3871633  0.5615481  1.2875390 -0.7154182 -0.9672472  1.0838814
    ## [13] -0.7758121 -0.4895676  0.4292619  0.2938984  0.7625819 -2.0182637
    ## [19] -1.1039454 -1.3230288 -1.3496117  0.5889913  1.1460627  0.5573350
    ## [25]  1.6840034

``` r
z_scores = function(x) {
  z = (x - mean(x)) / sd(x)
  z
}

z_scores(x_vec)
```

    ##  [1] -0.6679852 -0.6719874 -0.5788603  0.4292183 -0.1179703  1.5682129
    ##  [7]  0.3871633  0.5615481  1.2875390 -0.7154182 -0.9672472  1.0838814
    ## [13] -0.7758121 -0.4895676  0.4292619  0.2938984  0.7625819 -2.0182637
    ## [19] -1.1039454 -1.3230288 -1.3496117  0.5889913  1.1460627  0.5573350
    ## [25]  1.6840034

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = mean(x) / sd(x)
  
  z
}
```

# Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  list(mean = mean_x, 
       sd = sd_x)
}
mean_and_sd_2 = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

# Multiple inputs

``` r
sim_data = tibble(
  x = rnorm(30, mean = 2, sd = 3)
)

sim_data |> 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.33      2.99

``` r
sim_mean_sd = function(n, mu = 2, sigma = 3) {
  
  sim_data = tibble(
    x = rnorm(n, mean = mu, sd = sigma),
  )
  
  out_df = 
    sim_data |> 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
  return(out_df)
}
sim_mean_sd(n=30,mu=1,sigma=5)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1  0.217      4.88

# Revisiting examples

## LoTR

``` r
fellowship_ring = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship_ring")

two_towers = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers")

return_king = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king")

lotr_tidy = bind_rows(fellowship_ring, two_towers, return_king) |>
  janitor::clean_names() |>
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words") |> 
  mutate(race = str_to_lower(race)) |> 
  select(movie, everything()) 
```

Using functions

``` r
lotr_load_and_tidy = function(path, range, movie_name) {
    df = 
    readxl::read_excel(path, range = range) |>
    janitor::clean_names() |>
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words") |>
    mutate(
      race = str_to_lower(race),
      movie = movie_name) |> 
    select(movie, everything())
  
  df
}

lotr_tidy = 
  bind_rows(
    lotr_load_and_tidy("data/LotR_Words.xlsx", "B3:D6", "fellowship_ring"),
    lotr_load_and_tidy("data/LotR_Words.xlsx", "F3:H6", "two_towers"),
    lotr_load_and_tidy("data/LotR_Words.xlsx", "J3:L6", "return_king"))
```

## NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

``` r
nsduh_table <- function(html, table_num, table_name) {
  
  table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent),
      name = table_name) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  table
  
}

nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

nsduh_results = 
  bind_rows(
    nsduh_table(nsduh_html, 1, "marj_one_year"),
    nsduh_table(nsduh_html, 4, "cocaine_one_year"),
    nsduh_table(nsduh_html, 5, "heroin_one_year")
  )
```

# Functions as arguments

``` r
x_vec = rnorm(25, 0, 1)

my_summary = function(x, summ_func) {
  summ_func(x)
}

my_summary(x_vec, sd)
```

    ## [1] 0.8080395

``` r
my_summary(x_vec, IQR)
```

    ## [1] 0.8825714

``` r
my_summary(x_vec, var)
```

    ## [1] 0.6529278

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 3
f(x = y)
```

    ## [1] 6

``` r
x = 1
y = 2
f(x = y)
```

    ## [1] 4

``` r
x = 3
y = 2
f(x = y)
```

    ## [1] 4
