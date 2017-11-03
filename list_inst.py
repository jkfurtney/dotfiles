import boto3
import datetime
ec2 = boto3.resource('ec2')
client = boto3.client('cloudwatch')

instances = ec2.instances.filter(
    Filters=[{'Name': 'instance-state-name', 'Values': ['running']}])
for instance in instances:
    print instance.public_ip_address,
    print "({})".format(instance.id),
    print instance.instance_type,
    response = client.get_metric_statistics(
        Namespace='AWS/EC2',
        MetricName='CPUUtilization',
        Dimensions=[{'Name' : 'InstanceId', 'Value' : instance.id,},],
        StartTime=datetime.datetime.today()-datetime.timedelta(0,3*3600),
        EndTime=datetime.datetime.today(),
        Period=3600,
        Statistics=['Average',],
        Unit='Percent',
    )

    for datum in response['Datapoints']:
        print "{:0.0f}%".format(datum["Average"]),
    tags = instance.tags
    for tag in tags:
        if 'Key' in tag and tag['Key']=='Job':
            print "Job:",tag['Value'],
    print
