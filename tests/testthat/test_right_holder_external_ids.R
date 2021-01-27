context('right_holder_external_ids')

test_that('supported sources work', {
  allowed <- c('Abramus', 'Deezer', 'DicionarioMpb', 'Ecad', 'Facebook',
               'Instagram', 'Lastfm', 'LetrasComBr', 'MusicBrainz',
               'Napster', 'Shazam', 'Songkick', 'Spotify', 'Tratore',
               'Twitter', 'Vagalume', 'Youtube', 'RadiosComBr', 'TuneinRadio',
               'Playax', 'KnowledgeGraph')

  supported <- right_holder_external_ids() %>% supported_sources

  # Should have at least a few of the sources present.
  expect_gt(length(supported), 5)
  # They should all be within what we expect.
  expect_true(all(supported %in% allowed))
})

test_that('source filters work', {
  external_ids <- right_holder_external_ids() %>%
    for_right_holder('Anitta', 'Marília Mendonça', 'Tierry') %>%
    for_source('Spotify') %>%
    collect

  expect_equal(nrow(external_ids), 3)
  expect_equal(unique(external_ids$source_name), 'Spotify')
})
