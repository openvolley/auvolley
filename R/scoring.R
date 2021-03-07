#' Calculate individual player scores
#'
#' The Athletes Unlimited league plays matches of 3 sets, each to 25 points. To apply their scoring system to normal volleyball games, we need to decide what to do with sets 4 and 5 (if played). If you only want to count things that happened in the first 3 (or 4) sets, then you should remove data rows associated with sets 5 and/or 4 before calling this function. Otherwise, the `set_5_weighting` parameter allows the count of actions from set 5 to be scaled.
#'
#' Most of the actions that are counted are self-explanatory: serve aces, attack kills, and serve, pass, attack, and set errors. The perhaps-less obvious ones:
#' * A pass (serve reception) is counted if it is a perfect or positive pass
#' * A set assist is a set that leads to an attack kill
#' * Digs are counted on opposition attacks and block cover, if scouted, but not freeball passes. All dig grades except dig errors and digs directly back over the net are counted
#' * Only block kills are counted
#'
#' @references https://auprosports.com/volleyball/how-we-play-volleyball/
#' @param x datavolley or datavolleyplays: a datavolley object as returned by [datavolley::dv_read()], or the plays component of one
#' @param set_5_weighting numeric: the weighting that should be applied to actions from the fifth set of a match (if it went that long)
#' @param scoring list: the scores to apply for each outcome. See [au_scoring()] for details
#' @param infer_sets logical: set this to `TRUE` if the scout has not included all setting actions. Some scouts will not include setting actions unless the set is an error, or made by the non-designated setter. Using `infer_sets = TRUE` will insert a new setting action (made by the on-court setter) on attacks that don't have a corresponding set action scouted (excluding those in `no_set_attacks`)
#' @param no_set_attacks character: a vector of attack codes for which sets are not expected (setter dumps, attacks on overpasses, second-ball attacks)
#'
#' @return A tibble with a breakdown by player of points in each category, along with
#' * `points` : each player's total points scored. If set 5 results have been included, points scored in set 5 will be scaled by `set_5_weighting`
#' * `points_per_set`: each player's total points divided by the number of sets they played
#' * `adjusted_points`: each player's total points but adjusted for game length. The `adjusted_points` = `points_per_set` * 3 * `nmatches` * `playing_time` where `nmatches` is the average number of matches played by each team, and `playing_time` is (number of sets played by the player) / (number of sets played by their team)
#'
#' @seealso [au_scoring()]
#'
#' @examples
#' x <- datavolley::dv_read(datavolley::dv_example_file())
#' au_individual(x)
#'
#' @export
au_individual <- function(x, set_5_weighting = 25/15, scoring = au_scoring(), infer_sets = FALSE, no_set_attacks = c("PR", "PP", "P2")) {
    assert_that(is.numeric(set_5_weighting), set_5_weighting >= 0)
    if (inherits(x, c("datavolley", "peranavolley"))) x <- datavolley::plays(x)

    if (isTRUE(infer_sets)) {
        ## infer sets
        x <- au_insert_sets(x, no_set_attacks = no_set_attacks)
    }

    ## apply set weighting
    x <- mutate(dplyr::filter(x, !is.na(.data$set_number)), set_wt = case_when(.data$set_number > 4 ~ set_5_weighting, TRUE ~ 1.0))

    ## serving
    servex <- dplyr::filter(x, .data$skill == "Serve") %>% group_by(.data$player_id) %>%
        dplyr::summarize(aces = sumnna((.data$evaluation == "Ace") * .data$set_wt), serve_errors = sumnna((.data$evaluation == "Error") * .data$set_wt),
                         serve_points = sumnna(.data$aces * scoring$ace) + sumnna(.data$serve_errors * scoring$serve_error)) %>%
        ungroup

    ## passing
    passx <- dplyr::filter(x, .data$skill == "Reception") %>% group_by(.data$player_id) %>%
        dplyr::summarize(passes = sumnna(grepl("Perfect|Positive", .data$evaluation) * .data$set_wt), pass_errors = sumnna((.data$evaluation == "Error") * .data$set_wt),
                         pass_points = sumnna(.data$passes * scoring$pass) + sumnna(.data$pass_errors * scoring$pass_error)) %>%
        ungroup

    ## setting
    setx <- mutate(x, next_outcome = lead(.data$evaluation)) %>% dplyr::filter(.data$skill == "Set") %>% group_by(.data$player_id) %>%
        dplyr::summarize(assists = sumnna((.data$next_outcome == "Winning attack") * .data$set_wt), set_errors = sumnna(grepl("Error", .data$evaluation) * .data$set_wt),
                         set_points = sumnna(.data$assists * scoring$assist) + sumnna(.data$set_errors * scoring$set_error)) %>%
        ungroup

    ## attack
    attackx <- dplyr::filter(x, .data$skill == "Attack") %>% group_by(.data$player_id) %>%
        dplyr::summarize(kills = sumnna((.data$evaluation == "Winning attack") * .data$set_wt), attack_errors = sumnna((.data$evaluation == "Error") * .data$set_wt),
                         attack_points = sumnna(.data$kills * scoring$kill) + sumnna(.data$attack_errors * scoring$attack_error)) %>%
        ungroup

    ## block
    blockx <- dplyr::filter(x, .data$skill == "Block") %>% group_by(.data$player_id) %>%
        dplyr::summarize(blocks = sumnna((.data$evaluation == "Winning block") * .data$set_wt),
                         block_points = sumnna(.data$blocks * scoring$block))

    ## digs
    ## digs are also used for block cover, avoid these
    ##digx <- dplyr::filter(x, ((lag(.data$skill) == "Attack" & lag(.data$team) != .data$team) | (lag(.data$skill, 2) == "Attack" & lag(.data$team, 2) != .data$team & lag(.data$skill) == "Block" & lag(.data$team) == .data$team)) & .data$skill == "Dig" & !(.data$evaluation %in% c("Ball directly back over net", "Error") | grepl("block cover", .data$evaluation))) %>%
    ## no, count block cover too
    digx <- dplyr::filter(x, .data$skill == "Dig" & !(.data$evaluation %in% c("Ball directly back over net", "Error"))) %>%
        group_by(.data$player_id) %>%
        dplyr::summarize(digs = sumnna(.data$set_wt), dig_points = .data$digs * scoring$dig)

    ## count sets per player separately from skills
    temp <- dplyr::filter(x, !is.na(.data$player_id))
    temp <- bind_rows(lapply(1:6, function(p) {
        setNames(distinct(temp[, c("match_id", "set_number", "home_team_id", paste0("home_player_id", p))]), c("match_id", "set_number", "team_id", "player_id"))
    }),
    lapply(1:6, function(p) {
        setNames(distinct(temp[, c("match_id", "set_number", "visiting_team_id", paste0("visiting_player_id", p))]), c("match_id", "set_number", "team_id", "player_id"))
    }))
    ## liberos won't appear in that
    temp <- distinct(bind_rows(temp, dplyr::filter(x, !is.na(.data$player_name) & !is.na(.data$player_id)) %>% distinct(.data$match_id, .data$set_number, .data$team_id, .data$player_id)))

    ## all matches, n sets, with team_id
    xmt <- dplyr::summarize(group_by(na.omit(distinct(x, .data$match_id, .data$set_number, .data$team_id)), .data$match_id, .data$team_id), match_n_sets = max(.data$set_number))
    xmt <- dplyr::summarize(group_by(xmt, .data$team_id), team_sets_total = sum(.data$match_n_sets))

    ## by match, sets played per player and sets total in the match
    temp2 <- group_by(count(temp, .data$match_id, .data$team_id, .data$player_id, name = "sets_played"), .data$player_id, .data$team_id) %>%
        dplyr::summarize(sets_played = sum(.data$sets_played)) %>% ungroup
    temp2 <- left_join(temp2, xmt, by = "team_id")
    ## can't estimate team_sets_total for players that appear in multiple teams
    temp2$team_sets_total[temp2$player_id %in% temp2$player_id[duplicated(temp2$player_id)]] <- NA

    ## sets played per player and playing time of a player as (number of sets played by player) / (number of sets played by their team)
    temp <- dplyr::summarize(group_by(temp2, .data$player_id), prop_playing_time = sum(.data$sets_played) / sum(.data$team_sets_total), sets_played = sum(.data$sets_played))

    px <- dplyr::filter(x, !is.na(.data$player_name) & !is.na(.data$player_id)) %>% distinct(.data$player_name, .data$player_id) %>%
        left_join(temp, by = "player_id") %>%
        left_join(servex, by = "player_id") %>%
        left_join(passx, by = "player_id") %>%
        left_join(setx, by = "player_id") %>%
        left_join(attackx, by = "player_id") %>%
        left_join(blockx, by = "player_id") %>%
        left_join(digx, by = "player_id") %>%
        dplyr::filter(!is.na(.data$player_id))
    vrs <- c("aces", "serve_errors", "serve_points", "passes", "pass_errors", "pass_points", "assists", "set_errors", "set_points", "kills", "attack_errors", "attack_points", "blocks", "block_points", "digs", "dig_points")
    if (packageVersion("dplyr") >= "1.0.0") {
        px <- mutate(px, dplyr::across({{ vrs }}, na_to_0))
    } else {
        px <- dplyr::mutate_at(px, vrs, na_to_0)
    }
    px <- dplyr::arrange(mutate(px, points = .data$serve_points + .data$pass_points + .data$set_points + .data$attack_points + .data$block_points + .data$dig_points,
               points_per_set = .data$points / .data$sets_played), desc(.data$points_per_set))

    ## "adjusted" points, i.e. total points per player but normalized to 3-set matches, same number of matches per team, and average playing time per player
    ## average number of matches per team
    nmatches <- tryCatch(round(mean(pull(count(na.omit(distinct(x, .data$match_id, .data$team_id)), .data$team_id), .data$n))), error = function(e) 10)
    px <- mutate(px, adjusted_points = .data$points_per_set * 3 * nmatches * .data$prop_playing_time)
    px[, c(setdiff(names(px), c("prop_playing_time", "points_per_set")), "points_per_set")]

}

#' The default scores for each outcome
#'
#' @references https://auprosports.com/volleyball/how-we-play-volleyball/
#'
#' @return A named list with components `ace`, `serve_error`, `pass`, `pass_error`, `assist`, `set_error`, `kill`, `attack_error`, `block`, and `dig`, giving the numeric score for each
#'
#' @export
au_scoring <- function() {
    list(ace = 12, serve_error = -8, pass = 2, pass_error = -12, assist = 1, set_error = -12, kill = 8, attack_error = -12, block = 12, dig = 5)
}
