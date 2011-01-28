#
# Extra personal configuration organization
#
nuke.pluginAddPath('./gizmos')
nuke.pluginAddPath('./python')

#
# Example of automatic project-local customization loading
#
# baseDir = '/projects/'
# shows = os.listdir( baseDir )
# for s in shows:
#     showPath = os.path.join( '/projects', s, 'nuke')
#     if os.path.isdir( showPath ):
#         nuke.pluginAddPath( showPath )

# Or, if creating a facility-level config in /Library:
# # SOURCE USER'S DIRECTORY IF IT EXISTS
# user = os.getenv('USER')
# userPath = os.path.join( '/Library/NukeTools/Users', user)
# if os.path.isdir( userPath ):
#     nuke.pluginAddPath( userPath )
#
# # GET ALL SHOW DIRECTORIES
# baseDir = '/projects/'
# shows = os.listdir( baseDir )
# for s in shows:
#     showPath = os.path.join( '/projects', s, 'nuke')
#     if os.path.isdir( showPath ):
#         nuke.pluginAddPath( showPath )
