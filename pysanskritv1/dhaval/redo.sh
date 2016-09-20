echo "### Step 1: redo function directory"""
cd function
sh redo.sh
cd ../
echo "### Step 2: Analyses of verbdata roots"
cd roots
sh redo.sh
cd ../
echo "### Step 3: extract SanskritVerb conjugation tables"
cd conjtab
sh redo.sh
cd ../
echo "### Step 4: Compare SanskritVerb roots and  Pysan (MW) roots"
cd pysanroots
sh redo.sh
cd ../
echo "### Step 5: Compare SanskritVerb and Pysan conjugations"
cd pysanconjtab
sh redo.sh
cd ../
