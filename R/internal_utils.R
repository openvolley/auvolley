`%eq%` <- function (x, y) x == y & !is.na(x) & !is.na(y)

sumnna <- function(...) sum(..., na.rm = TRUE)

na_to_0 <- function(z) ifelse(is.na(z), 0, z)

## simple insertion of setter before attacks that have no set scouted
## no_set_attacks are attacks that don't need a set inserted before them
au_insert_sets <- function(x, no_set_attacks = c("PR", "PP", "P2")) {
    ridx_set <- mutate(x, rowN = row_number(),
                       add_set_before = case_when(.data$skill %eq% "Attack" & !(.data$attack_code %in% no_set_attacks) & !(lag(.data$skill) %eq% "Set") ~ TRUE,
                                                  TRUE ~ FALSE))
    ridx <- ridx_set$rowN[which(ridx_set$add_set_before)]
    if (length(ridx) > 0) {
        x <- ovlytics::ov_augment_plays(x, to_add = "setters")
        x <- mutate(x, tmp_row_number = row_number())
        xset <- dplyr::filter(x, .data$tmp_row_number %in% ridx) %>%
            mutate(player_id = .data$setter_id, skill = "Set", evaluation_code = "+", evaluation = "Positive", tmp_row_number = .data$tmp_row_number - 0.5) %>%
            dplyr::select("match_id", "set_number", "player_id", "skill", "evaluation_code", "evaluation", "tmp_row_number")
        x <- dplyr::select(dplyr::arrange(bind_rows(x, xset), .data$tmp_row_number), -"tmp_row_number")
    }
    x
}
