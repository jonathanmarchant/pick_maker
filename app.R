library(shiny)
library(bslib)
library(tidyverse)
library(espnscrapeR)
source("helpers.R")

param_season <- 2025

divisions <- tribble(
  ~team,~division,~abbreviation,
"Buffalo Bills","AFC East","BUF",
"Miami Dolphins","AFC East","MIA",
"New England Patriots","AFC East","NE",
"New York Jets","AFC East","NYJ",
"Baltimore Ravens","AFC North","BAL",
"Cincinnati Bengals","AFC North","CIN",
"Cleveland Browns","AFC North","CLE",
"Pittsburgh Steelers","AFC North","PIT",
"Houston Texans","AFC South","HOU",
"Indianapolis Colts","AFC South","IND",
"Jacksonville Jaguars","AFC South","JAX",
"Tennessee Titans","AFC South","TEN",
"Denver Broncos","AFC West","DEN",
"Kansas City Chiefs","AFC West","KC",
"Las Vegas Raiders","AFC West","LV",
"Los Angeles Chargers","AFC West","LAC",
"Dallas Cowboys","NFC East","DAL",
"New York Giants","NFC East","NYG",
"Philadelphia Eagles","NFC East","PHI",
"Washington Commanders","NFC East","WSH",
"Chicago Bears","NFC North","CHI",
"Detroit Lions","NFC North","DET",
"Green Bay Packers","NFC North","GB",
"Minnesota Vikings","NFC North","MIN",
"Atlanta Falcons","NFC South","ATL",
"Carolina Panthers","NFC South","CAR",
"New Orleans Saints","NFC South","NO",
"Tampa Bay Buccaneers","NFC South","TB",
"Arizona Cardinals","NFC West","ARI",
"Los Angeles Rams","NFC West","LAR",
"San Francisco 49ers","NFC West","SF",
"Seattle Seahawks","NFC West","SEA",
)

# Cache data on app load
schedule <- jm_nfl_schedule(season = param_season)
standings <- get_nfl_standings(season = param_season)
qbr_data <- get_nfl_qbr(season = param_season)
qb_stats <- scrape_espn_stats(season = param_season, stats = "passing", season_type = "Regular")

# Pre-process standings with division rank
standings_with_rank <- standings |>
  left_join(divisions, by = c("team_abb" = "abbreviation")) |>
  group_by(division) |>
  mutate(division_rank = rank(seed, ties.method = "first")) |>
  ungroup()

schedule <- schedule |>
  mutate(game_date = as.Date(game_date))

get_this_weeks_games <- function() {
  today <- Sys.Date()
  days_since_wed <- (as.numeric(format(today, "%u")) - 3) %% 7
  most_recent_wed <- today - days_since_wed
  week_end <- most_recent_wed + 7
  
  this_week <- schedule |>
    filter(game_date >= most_recent_wed & game_date < week_end)
  
  games <- setNames(this_week$game_id, 
                    paste(this_week$away_team_abb, "@", this_week$home_team_abb))
  
  games
}

ui <- page_sidebar(
  title = "NFL Games This Week",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#013369",
    secondary = "#d50a0a"
  ),
  sidebar = sidebar(
    width = 300,
    selectInput(
      "selected_game",
      "Select a game:",
      choices = get_this_weeks_games()
    )
  ),
  tags$style(HTML("
    .highlight-row {
      background-color: #fff3cd !important;
      font-weight: bold;
    }
  ")),
  htmlOutput("game_info")
)

server <- function(input, output, session) {
  output$game_info <- renderUI({
    game <- schedule |>
      filter(game_id == input$selected_game)
    
    away_standing <- standings_with_rank |>
      filter(team_abb == game$away_team_abb)
    
    home_standing <- standings_with_rank |>
      filter(team_abb == game$home_team_abb)
    
    # Division standings tables as HTML with highlighting
    create_standings_table <- function(division_name, highlight_team) {
      div_standings <- standings_with_rank |>
        filter(division == division_name) |>
        arrange(division_rank) |>
        mutate(across(c(wins, losses, ties, pts_for, pts_against), as.integer))
      
      table_html <- '<table class="table table-striped table-hover table-bordered table-sm">'
      table_html <- paste0(table_html, '<thead><tr><th>Team</th><th>W</th><th>L</th><th>T</th><th>Pts For</th><th>Pts Against</th></tr></thead><tbody>')
      
      for (i in 1:nrow(div_standings)) {
        row_class <- if (div_standings$team_abb[i] == highlight_team) "highlight-row" else ""
        table_html <- paste0(table_html, 
          '<tr class="', row_class, '">',
          '<td>', div_standings$team_abb[i], '</td>',
          '<td>', div_standings$wins[i], '</td>',
          '<td>', div_standings$losses[i], '</td>',
          '<td>', div_standings$ties[i], '</td>',
          '<td>', div_standings$pts_for[i], '</td>',
          '<td>', div_standings$pts_against[i], '</td>',
          '</tr>')
      }
      
      table_html <- paste0(table_html, '</tbody></table>')
      HTML(table_html)
    }
    
    # Get team game history with dates and colors
    create_game_history <- function(team_abb) {
      games <- schedule |>
        filter(away_team_abb == team_abb | home_team_abb == team_abb) |>
        filter(season == param_season) |>
        filter(slug == "regular-season") |>
        mutate(
          opponent = if_else(away_team_abb == team_abb, home_team_abb, away_team_abb),
          home_away_symbol = if_else(away_team_abb == team_abb, "@", "vs"),
          result = case_when(
            is.na(home_score) | status_name == "STATUS_SCHEDULED" ~ "",
            away_team_abb == team_abb & away_score > home_score ~ "W",
            home_team_abb == team_abb & home_score > away_score ~ "W",
            away_score == home_score ~ "T",
            TRUE ~ "L"
          ),
          score = if_else(is.na(home_score), "",
                         if_else(away_team_abb == team_abb,
                                paste(away_score, "-", home_score),
                                paste(home_score, "-", away_score))),
          result_class = case_when(
            result == "W" ~ "table-success",
            result == "L" ~ "table-danger",
            result == "T" ~ "table-warning",
            TRUE ~ ""
          )
        ) |>
        left_join(
          standings |> select(opponent = team_abb, opponent_record = record),
          by = "opponent"
        ) |>
        mutate(opp_with_record = str_c(home_away_symbol, " ", opponent, " (", opponent_record, ")")) |>
        arrange(game_date)
      
      table_html <- '<table class="table table-striped table-hover table-bordered table-sm">'
      table_html <- paste0(table_html, '<thead><tr><th>Date</th><th>Opponent</th><th>Result</th><th>Score</th></tr></thead><tbody>')
      
      for (i in 1:nrow(games)) {
        table_html <- paste0(table_html,
          '<tr class="', games$result_class[i], '">',
          '<td>', format(games$game_date[i], "%m/%d"), '</td>',
          '<td>', games$opp_with_record[i], '</td>',
          '<td>', games$result[i], '</td>',
          '<td>', games$score[i], '</td>',
          '</tr>')
      }
      
      table_html <- paste0(table_html, '</tbody></table>')
      HTML(table_html)
    }
    
    # Get QB details combining QBR and passing stats
    get_qb_info <- function(team_abb) {
      qb <- qbr_data |>
        filter(team_abb == !!team_abb) |>
        arrange(desc(qbr_total)) |>
        slice(1)
      
      if (nrow(qb) > 0) {
        # Try to match QB with passing stats
        qb_passing <- qb_stats |>
          filter(team == qb$team) |>
          slice(1)
        
        list(qb = qb, stats = qb_passing)
      } else {
        list(qb = NULL, stats = NULL)
      }
    }
    
    away_qb_info <- get_qb_info(game$away_team_abb)
    home_qb_info <- get_qb_info(game$home_team_abb)
    
    tagList(
      card(
        card_header(
          class = "bg-primary text-white",
          h4(class = "mb-0", paste(game$away_team_abb, "@", game$home_team_abb))
        ),
        layout_columns(
          col_widths = c(6, 6),
          p(strong("Game ID: "), input$selected_game),
          p(strong("Date: "), format(game$game_date, "%B %d, %Y"))
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        # Away Team Column
        card(
          card_header(
            class = "bg-light",
            h5(class = "mb-0", paste("🏈", away_standing$team_full))
          ),
          if(!is.null(away_qb_info$qb)) {
            card_body(
              h6(class = "text-muted", "Top Quarterback"),
              p(strong(away_qb_info$qb$name_display)),
              p("QBR: ", span(class = "badge bg-primary", round(away_qb_info$qb$qbr_total, 1))),
              if (!is.null(away_qb_info$stats) && nrow(away_qb_info$stats) > 0) {
                tagList(
                  p("Passing Yards: ", away_qb_info$stats$yds),
                  p("Touchdowns: ", away_qb_info$stats$td),
                  p("Completion %: ", round(away_qb_info$stats$cmp_pct, 1), "%"),
                  p("Passer Rating: ", round(away_qb_info$stats$qbr, 1))
                )
              }
            )
          },
          card_footer(
            h6(paste(away_standing$division, "Standings")),
            create_standings_table(away_standing$division, game$away_team_abb)
          ),
          card_footer(
            h6("Season Games"),
            create_game_history(game$away_team_abb)
          )
        ),
        
        # Home Team Column
        card(
          card_header(
            class = "bg-light",
            h5(class = "mb-0", paste("🏠", home_standing$team_full))
          ),
          if(!is.null(home_qb_info$qb)) {
            card_body(
              h6(class = "text-muted", "Top Quarterback"),
              p(strong(home_qb_info$qb$name_display)),
              p("QBR: ", span(class = "badge bg-primary", round(home_qb_info$qb$qbr_total, 1))),
              if (!is.null(home_qb_info$stats) && nrow(home_qb_info$stats) > 0) {
                tagList(
                  p("Passing Yards: ", home_qb_info$stats$yds),
                  p("Touchdowns: ", home_qb_info$stats$td),
                  p("Completion %: ", round(home_qb_info$stats$cmp_pct, 1), "%"),
                  p("Passer Rating: ", round(home_qb_info$stats$qbr, 1))
                )
              }
            )
          },
          card_footer(
            h6(paste(home_standing$division, "Standings")),
            create_standings_table(home_standing$division, game$home_team_abb)
          ),
          card_footer(
            h6("Season Games"),
            create_game_history(game$home_team_abb)
          )
        )
      )
    )
  })
}

shinyApp(ui, server)