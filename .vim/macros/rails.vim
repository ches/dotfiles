" rails.vim will source this file on init for all its file types
" TODO: do we need to Rset any custom file alternates, etc.?
" TODO: can try to get fancy to conditionally override defaults...
" Rnavcommand observer app/observers -glob=**/*
Rnavcommand sweeper app/sweepers -glob=**/*
Rnavcommand decorator app/decorators -glob=**/*
Rnavcommand job app/jobs -glob=**/*
Rnavcommand admin app/admin -default=both()
Rnavcommand factory spec/factories -default=model()
Rnavcommand blueprint spec/support/blueprints -default=model()
Rnavcommand uploader app/uploaders -glob=**/* -suffix=_uploader.rb -default=model()
Rnavcommand service app/services -glob=**/*

