/\[/                { print ("const vectors = [") }
/]/                 { print ("]") }
/"hash": "blake2s"/ { print ("\tTestVector{") }
/"in":/             { gsub(/"|,/, "", $2); print ("\t\tinput: '" $2 "',") }
/"key":/            { gsub(/"|,/, "", $2); print ("\t\tkey: '" $2 "',") }
/"out":/            { gsub(/"|,/, "", $2); print ("\t\toutput: '" $2 "',") }
/}/                 { print ("    },") }
