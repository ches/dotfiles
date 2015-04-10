# Just source this to set the ENV vars.

# Account switcher in the style of rbenv.
# See https://github.com/michaelcontento/awsenv and my fork
if [ -d ~/.awsenv/bin ]; then
    export PATH=$PATH:$HOME/.awsenv/bin
    eval "$(awsenv init -)"
fi

# The huge legacy Java tools. awscli is rather complete now, has good shell
# completion, and is self-documenting. Use that, really.
# if [[ "$(uname -s)" = "Darwin" ]]; then
#     export EC2_HOME="/usr/local/Library/LinkedKegs/ec2-api-tools/jars"
#     export AWS_IAM_HOME="/usr/local/Cellar/aws-iam-tools/HEAD/jars"
#     export AWS_CLOUDWATCH_HOME="/usr/local/Library/LinkedKegs/cloud-watch/jars"
#     export AWS_AUTO_SCALING_HOME="/usr/local/Library/LinkedKegs/auto-scaling/jars"
#     export AWS_CLOUDFORMATION_HOME="/usr/local/Library/LinkedKegs/aws-cfn-tools/jars"
# fi

# export SERVICE_HOME="$AWS_CLOUDWATCH_HOME"

