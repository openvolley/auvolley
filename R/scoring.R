#' Calculate individual player scores
#'
#' The Athletes Unlimited league plays matches of 3 sets, each to 25 points. To apply their scoring system to normal volleyball games, we need to decide what to do with sets 4 and 5 (if played). If you only want to count things that happened in the first 3 (or 4) sets, then you should remove data rows associated with sets 5 and/or 4 before calling this function. Otherwise, the `set_5_weighting` parameter allows the count of actions from set 5 to be scaled.
#'
#' @references https://auprosports.com/volleyball/how-we-play-volleyball/
#' @param x datavolley or datavolleyplays: a datavolley object as returned by [datavolley::dv_read()], or the plays component of one
#' @param set_5_weighting numeric: the weighting that should be applied to actions from the fifth set of a match (if it went that long)
#' @param scoring list: the scores to apply for each outcome. See [au_scoring()] for details
#' @param infer_sets logical: set this to `TRUE` if the scout has not included all setting actions. Some scouts will not include setting actions unless the set is an error, or made by the non-designated setter. Using `infer_sets = TRUE` will insert a new setting action (made by the on-court setter) on attacks that don't have a corresponding set action scouted (excluding those in `no_set_attacks`)
#' @param no_set_attacks character: a vector of attack codes for which sets are not expected (setter dumps, attacks on overpasses, second-ball attacks)
#'
#' @return A tibble
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
        dplyr::summarize(assists = sumnna((.data$next_outcome == "Winning attack") * .data$set_wt), set_errors = sumnna((.data$evaluation == "Error") * .data$set_wt),
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
    digx <- dplyr::filter(x, ((lag(.data$skill) == "Attack" & lag(.data$team) != .data$team) | (lag(.data$skill, 2) == "Attack" & lag(.data$team, 2) != .data$team & lag(.data$skill) == "Block" & lag(.data$team) == .data$team)) & .data$skill == "Dig" & !(.data$evaluation %in% c("Ball directly back over net", "Error") | grepl("block cover", .data$evaluation))) %>%
        group_by(.data$player_id) %>%
        dplyr::summarize(digs = sumnna(.data$set_wt), dig_points = .data$digs * scoring$dig)

    ## count sets per player separately from skills
    px <- dplyr::filter(x, !is.na(.data$player_name) & !is.na(.data$player_id)) %>% group_by(.data$player_name, .data$player_id) %>% distinct(.data$match_id, .data$set_number) %>% dplyr::summarize(sets_played = n()) %>%
        ## TODO: weight by team sets, not individual? but count sets when on court, not having made an action, anyway
        left_join(servex, by = c("player_id")) %>%
        left_join(passx, by = c("player_id")) %>%
        left_join(setx, by = c("player_id")) %>%
        left_join(attackx, by = c("player_id")) %>%
        left_join(blockx, by = c("player_id")) %>%
        left_join(digx, by = c("player_id")) %>%
        dplyr::filter(!is.na(.data$player_id))
    vrs <- c("aces", "serve_errors", "serve_points", "passes", "pass_errors", "pass_points", "assists", "set_errors", "set_points", "kills", "attack_errors", "attack_points", "blocks", "block_points", "digs", "dig_points")
    if (packageVersion("dplyr") >= "1.0.0") {
        px <- mutate(px, dplyr::across({{ vrs }}, na_to_0))
    } else {
        px <- dplyr::mutate_at(px, vrs, na_to_0)
    }
    px %>% mutate(points = .data$serve_points + .data$pass_points + .data$set_points + .data$attack_points + .data$block_points + .data$dig_points,
               points_per_set = .data$points / .data$sets_played) %>%
        dplyr::arrange(desc(.data$points_per_set))

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
