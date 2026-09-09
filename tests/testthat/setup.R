# Keep the resolve cache out of the user's real cache dir during tests.
withr::local_options(
  list(medrev.cache_dir = file.path(tempdir(), "medrev-test-cache")),
  .local_envir = teardown_env()
)
