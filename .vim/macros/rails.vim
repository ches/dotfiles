" rails.vim will source this file on init for all its file types

" See |g:rails_projections|
if !exists('g:rails_projections')
  let g:rails_projections = {
        \ 'app/services/*.rb' : { 'command' : 'service' },
        \ 'app/sweepers/*.rb' : { 'command' : 'sweeper' }
        \ }
endif

" See |g:rails_gem_projections|
if !exists('g:rails_gem_projections')
  let g:rails_gem_projections = {
        \ 'activeadmin' : {
        \   'app/admin/*.rb' : { 'command' : 'admin' }
        \ },
        \ 'carrierwave' : {
        \   'app/uploaders/*_uploader.rb' : {
        \     'command' : 'uploader',
        \     'keywords' : 'process version',
        \     'template' : ['class %SUploader < CarrierWave::Uploader::Base', 'end'],
        \     'test' : [
        \       'test/unit/%s_uploader_test.rb',
        \       'spec/models/%s_uploader_spec.rb'
        \     ]
        \   }
        \ },
        \ 'draper' : {
        \   'app/decorators/*_decorator.rb' : {
        \     'command' : 'decorator',
        \     'affinity' : 'model',
        \     'test' : 'spec/decorators/%s_decorator_spec.rb'
        \   }
        \ },
        \ 'machinist' : {
        \   'spec/support/blueprints.rb' : { 'command' : 'blueprint' },
        \   'spec/blueprints/*.rb' : {
        \     'command' : 'blueprint',
        \     'affinity' : 'model',
        \     'alternate' : 'app/models/%s.rb',
        \     'related' : 'db/schema.rb#%p',
        \     'test' : 'spec/models/%s_spec.rb'
        \   }
        \ }}
endif

