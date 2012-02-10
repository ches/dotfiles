# Just source this to set the ENV vars.
# Set AWS_CREDENTIAL_FILE in ~/.local

export EC2_PRIVATE_KEY="$(/bin/ls $HOME/.ec2/pk-*.pem)"
export EC2_CERT="$(/bin/ls $HOME/.ec2/cert-*.pem)"

if [ "$(uname -s)" == "Darwin" ]; then
    export EC2_HOME="/usr/local/Cellar/ec2-api-tools/1.5.2.3/jars"
    export AWS_IAM_HOME="/usr/local/Cellar/aws-iam-tools/HEAD/jars"
    export AWS_CLOUDWATCH_HOME="/usr/local/Cellar/cloud-watch/1.0.12.1/jars"
    export AWS_AUTO_SCALING_HOME="/usr/local/Cellar/auto-scaling/1.0.49.1/jars"
    export AWS_CLOUDFORMATION_HOME="/usr/local/Cellar/aws-cfn-tools/1.0.8/jars"
fi

export SERVICE_HOME="$AWS_CLOUDWATCH_HOME"

