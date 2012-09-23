# Just source this to set the ENV vars.
# Set AWS_CREDENTIAL_FILE in ~/.local

export EC2_PRIVATE_KEY="$(/bin/ls $HOME/.ec2/pk-*.pem)"
export EC2_CERT="$(/bin/ls $HOME/.ec2/cert-*.pem)"

if [ "$(uname -s)" == "Darwin" ]; then
    export EC2_HOME="/usr/local/Library/LinkedKegs/ec2-api-tools/jars"
    export AWS_IAM_HOME="/usr/local/Cellar/aws-iam-tools/HEAD/jars"
    export AWS_CLOUDWATCH_HOME="/usr/local/Library/LinkedKegs/cloud-watch/jars"
    export AWS_AUTO_SCALING_HOME="/usr/local/Library/LinkedKegs/auto-scaling/jars"
    export AWS_CLOUDFORMATION_HOME="/usr/local/Library/LinkedKegs/aws-cfn-tools/jars"
fi

export SERVICE_HOME="$AWS_CLOUDWATCH_HOME"

