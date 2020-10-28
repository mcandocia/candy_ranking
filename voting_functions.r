

build_like_matrix <- function(data, weight_col=NA){
  data = data %>% select_at(
    vars(
      -matches('^Rank.*')
    )
  )

  if (is.na(weight_col)){
    data$weight = 1
  } else {
    data$weight = data[,weight_col,T]
  }

  molten_data = data %>%
    group_by_at(
      vars(-matches('^Like|weight'))
    ) %>%
    summarize_at(
      vars(matches('^Like')),
      list(
        like_count = ~ sum(weight*((.)==1), na.rm=T),
        dislike_count = ~ sum(weight*((.)==-1), na.rm=T)
      ),
      .groups='drop'
    ) %>%
    reshape2::melt() %>%
    filter(
      grepl('^Like', variable)
    ) %>%
    mutate(
      is_like = grepl('_like', variable),
      varname = gsub('_(like|dislike)_count','', variable)
    ) %>%
    select(-variable) %>%
    reshape2::dcast(varname ~ is_like) %>%
    rename(n_likes=`TRUE`, n_dislikes=`FALSE`) %>%
    mutate(
      likes_ranking = rank(-n_likes, ties.method='min'),
      dislikes_ranking = rank(n_dislikes, ties.method='min'),
      likes_dislikes_difference = n_likes - n_dislikes,
      likes_dislikes_ranking = rank(-likes_dislikes_difference, ties.method='min')
    ) %>%
    arrange(likes_ranking, dislikes_ranking) %>%
    rename(
      Candy=varname
    ) %>%
    mutate(
      Candy=gsub('^Like ','', Candy)
    )

  group_sizes = data %>%
    group_by_at(
      vars(-matches('^Like'))
    ) %>%
    summarize(count=sum(weight),
              .groups='drop')

  return(list(
    molten_data=molten_data,
    group_sizes=group_sizes
  ))
}

build_rank_matrix <- function(data, max_rank=5, weight_col=NA){
  if (is.na(weight_col)){
    weight_col = 'weight'
    data$weight=1
  } else {
    data$weight = data[,weight_col,T]
  }
  data = data %>% select_at(
    vars(
      -matches('^Like.*')
    )
  )

  molten_data = data %>%
    group_by_at(
      vars(-matches('^Rank|^weight$'))
    ) %>%
    summarize_at(
      vars(matches('^Rank')),
      list(
        r1_count = ~ sum(((.)==1)*weight, na.rm=T),
        r2_count = ~ sum(((.)==2)*weight, na.rm=T),
        r3_count = ~ sum(((.)==3)*weight, na.rm=T),
        r4_count = ~ sum(((.)==4)*weight, na.rm=T),
        r5_count = ~ sum(((.)==5)*weight, na.rm=T)
      ),
      .groups='drop'
    ) %>%
    reshape2::melt() %>%
    mutate(
      rank = as.numeric(gsub('.*r(\\d).*','\\1', variable)),
      varname = gsub('_(r\\d)_count','', variable)
    ) %>%
    filter(
      rank <= max_rank
    )

  group_sizes = data %>%
    group_by_at(
      vars(-matches('^Rank'))
    ) %>%
    summarize(count=sum(weight),
              .groups='drop')

  return(list(
    molten_data=molten_data,
    group_sizes=group_sizes
  ))
}

dowell_count <- function(x) (2^(-x))

#' Borda count algorithm
build_borda_count_matrix <- function(data, weight_col=NA, rank_to_score_func = function(x){6-x}){
  if (is.na(weight_col)){
    weight_col = 'weight'
    data$weight=1
  }
  data = data %>% select_at(
    vars(
      -matches('^Like.*')
    )
  )

  molten_data = data %>%
    group_by_at(
      vars(-matches('^Rank|^weight$'))
    ) %>%
    summarize_at(
      vars(matches('^Rank.*')),
      list(~sum(weight * rank_to_score_func(.), na.rm=T)),
      .groups='drop'
    ) %>%
    reshape2::melt() %>%
    mutate(
      variable=gsub('Rank_','', variable),
      borda_ranking=rank(-value, ties.method='min')
    ) %>%
    rename(
      borda_count=value,
      Candy=variable
    ) %>%
    mutate(
      Candy = gsub('^Like ','',Candy)
    ) %>%
    arrange(borda_ranking) %>%
    mutate(
      Candy=gsub('^Rank ','', Candy)
    )

  group_sizes = data %>%
    group_by_at(
      vars(-matches('^Rank'))
    ) %>%
    summarize(count=sum(weight),
              .groups='drop')

  return(
    list(
      molten_data=molten_data,
      group_sizes=group_sizes
    )
  )
}

shift_rankings <- function(data){
  rank_vars = names(data)[grepl('^Rank', names(data))]
  data[,rank_vars] = rowwise(
    data[,rank_vars]
  ) %>% do(
    data.frame(t(rank(unlist(.)) *  sign(unlist(.))))
  )
  return(data)
}

#' Iterate through rankings of data via runoff voting
#'
#'
rank_iterate <- function(data, max_iter=Inf, top_n=5, weight_modifier_func=function(x, group_sizes) identity(x), verbose=FALSE, weight_col=NA){
  # note: if max_iter = 1, method is plurality vote and no rows are removed
  iter = 0
  while (iter < max_iter){
    iter = iter + 1
    # tabulate
    #print('Shifting')
    data = data %>% shift_rankings()
    #print('Summarizing')
    group_summary = build_rank_matrix(data, weight_col=weight_col)
    rank_mat = group_summary$molten_data
    group_sizes = group_summary$group_sizes

    # this will weight each group, and then summarize all the groups again to achieve a total aggregate
    rank_mat = weight_modifier_func(rank_mat, group_sizes)

    #print(rank_mat %>% filter(value==0))

    min_cols = with(
      rank_mat %>% filter(rank==1),
      varname[value==min(value)]
    )
    #print(min_cols)
    if (length(min_cols) + top_n > nrow(rank_mat %>% filter(rank==1))){
      break
    } else if (top_n == 1 & nrow(rank_mat %>% filter(rank==1))==1){
      break
    }

    #print(nrow(rank_mat %>% filter(rank==1)))
    data = data %>%
      select_at(vars(-one_of(min_cols)))

    if (verbose){
      print(sprintf('Removed %s', paste(min_cols, collapse=';')))
    }
  }

  return(
    rank_mat %>% filter(rank==1) %>%
      arrange(-value) %>%
      mutate(
        runoff_ranking=rank(-value, ties.method='min')
      ) %>%
      rename(
        Candy=variable
      ) %>%
      mutate(
        Candy=gsub('^Rank ','', Candy),
        Candy=gsub('_r\\d+_count','', Candy)
      ) %>%
      select(-varname, -rank)
  )
}

as_electoral <- function(func, data, key=NA, ...){
  states = unique(data$State)
  states = states[!states %in% c('Other / Not Applicable')]
  vote_list = list()
  for (state in states){
    #print(state)
    n_votes = electoral_votes_df %>%
      filter(State==state) %>%
      pull(Votes)

    result_df = func(data %>% filter(State==state) %>% select(-State), ...)
    if (!is.data.frame(result_df))
      result_df = result_df$molten_data

    if (is.na(key)){
      result=result_df$Candy[1]
    } else {
      result = result_df[result_df[,key,T]==1,'Candy',T]
    }
    wgt = 1/length(result)
    #print(wgt)
    for (r in result){
      if (!r %in% names(vote_list)){
        vote_list[[r]] = n_votes*wgt
      } else {
        vote_list[[r]] = vote_list[[r]] + n_votes*wgt
      }
    }
  }
  return(
    data.frame(
      Candy=names(vote_list),
      electoral_votes=unlist(vote_list)
     )%>%
      mutate(
        electoral_vote_ranking=rank(-electoral_votes, ties.method='min')
      ) %>%
      arrange(electoral_vote_ranking)
  )
}

as_house <- function(func, data, key=NA, ...){
  states = unique(data$State)
  states = states[!states %in% c('Other / Not Applicable','District of Columbia')]
  vote_list = list()
  for (state in states){
    #print(state)
    n_votes = house_votes_df %>%
       filter(State==state) %>%
      pull(Votes)

    result_df = func(data %>% filter(State==state) %>% select(-State), ...)
    #print(result_df)
    if (!is.data.frame(result_df))
      result_df = result_df$molten_data


    if (is.na(key)){
      result=result_df$Candy[1]
    } else {
      result = result_df[result_df[,key,T]==1,'Candy',T]
    }
    wgt = 1/length(result)
    #print(wgt)
    for (r in result){
      if (!r %in% names(vote_list)){
        vote_list[[r]] = n_votes*wgt
      } else {
        vote_list[[r]] = vote_list[[r]] + n_votes*wgt
      }
    }
  }
  return(
    data.frame(
      Candy=names(vote_list),
      house_votes=unlist(vote_list)
    ) %>%
      mutate(
        house_vote_ranking=rank(-house_votes, ties.method='min')
      ) %>%
      arrange(house_vote_ranking)
  )
}


as_senatorial <- function(func, data, key=NA, ...){
  states = unique(data$State)
  states = states[!states %in% c('Other / Not Applicable','District of Columbia')]
  vote_list = list()
  for (state in states){
    #print(state)
    n_votes = 2

    result_df = func(data %>% filter(State==state) %>% select(-State), ...)
    #print(result_df)
    if (!is.data.frame(result_df))
      result_df = result_df$molten_data


    if (is.na(key)){
      result=result_df$Candy[1]
    } else {
      result = result_df[result_df[,key,T]==1,'Candy',T]
    }
    wgt = 1/length(result)
    #print(wgt)
    for (r in result){
      if (!r %in% names(vote_list)){
        vote_list[[r]] = n_votes*wgt
      } else {
        vote_list[[r]] = vote_list[[r]] + n_votes*wgt
      }
    }
  }
  #print(length(vote_list))
  return(
    data.frame(
      Candy=names(vote_list),
      senate_votes=unlist(vote_list)
    ) %>%
      mutate(
        senate_vote_ranking=rank(-senate_votes, ties.method='min')
      ) %>%
      arrange(senate_vote_ranking)
  )
}

# apply a function by US state
statewise <- function(func, data,  ...){
  states = unique(data$State)
  res_list = list()
  for (state in states){
    res = func(data %>% filter(State==state) %>% select(-State), ...)
    if (!is.data.frame(res)){
      res = res$molten_data
    }
    res$State = state
    res_list[[state]] = res
  }
  return(bind_rows(res_list))
}

# tiebreaker indicator

flag_ties <- function(data, rank_col){

  if ('state' %in% names(data)){
    rename_state=TRUE
    data = data %>% rename(State=state)
  } else {
    rename_state=FALSE
  }
  tied_states = data %>%
    group_by(State) %>%
    summarize(n_rank1=sum(1==!!sym(rank_col))) %>%
    filter(n_rank1 > 1) %>%
    pull(State)

  data = data %>%
    mutate(
      Candy = ifelse(
        State %in% tied_states,
        'Tied',
        Candy
      )
    ) %>%
    group_by(State) %>%
    filter(
      !duplicated(Candy)
    ) %>%
    ungroup()

  if (rename_state){
    data = data %>% rename(state=State)
  }

  data
}

# should have 2 columns: Candy, ranking
flag_ties_nationalized <- function(data, rank_col, ranking_data){
  ranking_data = ranking_data %>%
    transmute(
      Candy=Candy,
      added_score = 0.01/ranking
    )

  if ('state' %in% names(data)){
    rename_state=TRUE
    data = data %>% rename(State=state)
  } else {
    rename_state=FALSE
  }
  tied_states = data %>%
    group_by(State) %>%
    summarize(n_rank1=sum(1==!!sym(rank_col))) %>%
    filter(n_rank1 > 1) %>%
    pull(State)

  data = data %>%
    left_join(
      ranking_data,
      by='Candy'
    ) %>%
    group_by(State) %>%
    mutate(
      !!sym(rank_col):=rank(!!sym(rank_col)+added_score, ties.method='min')
    ) %>%
    ungroup() %>%
    select(-added_score)

  if (rename_state){
    data = data %>% rename(state=State)
  }
  data
}


if (FALSE){
  # test functions
  rank_iterate(candy_df %>% select(4:90))

  rank_iterate(candy_df %>% select(4:90), top_n=2)

  build_borda_count_matrix(candy_df %>% select(4:90))

  build_like_matrix(candy_df %>% select(4:90))
}
