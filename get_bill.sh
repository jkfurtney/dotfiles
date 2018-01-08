OBJ=s3://itascabillinglog/{}-aws-billing-csv-$(date +%Y-%m).csv
echo "getting "$OBJ
aws s3 cp $OBJ bill.csv
foo=`tail -2 bill.csv | head -1`
echo ${foo##*,}
