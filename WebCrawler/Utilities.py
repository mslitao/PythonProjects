#! /usr/bin/env python

# Author: Srinivas Vadrevu
# Contact srivad@microsoft.com if you have any questions

import sys,os,time,re,threading,datetime
from string import *


class RotateStruct:
	colID = -1
	outputStr = ""


class RotateThread ( threading.Thread ):

	def __init__ (self, inputFile, delimiter, rotateStruc):
		self.inputFile = inputFile
		self.delimiter = delimiter
		self.rotateStruc = rotateStruc
		threading.Thread.__init__ ( self )
	
	def run ( self ):
		fp = open(self.inputFile, 'r')
		currStr = ""
		for line in fp:
			arr = split(line.strip(), self.delimiter)
			self.rotateStruc.outputStr += (arr[self.rotateStruc.colID] + ',')
		fp.close()
		

class utils:

	def __init__(self):
		self.temp = ""


	# Parse command line arguments, and return arguments as an array
	def parseCmdLineArgs(numArgs, usage):
		if len(sys.argv) != numArgs+1:
			sys.stderr.write(usage + "\n")
			sys.exit()

		return sys.argv[1:numArgs+1]

	# Print Elapsed time, given start time
	def printElapsedTime(startTime):
		endTime = time.time()
		elapsedTime = endTime - startTime
		secs = int(elapsedTime)
		ms = str(elapsedTime - secs)[2:6]
		mins = secs / 60
		secs = secs % 60
		hours = mins / 60
		mins = mins % 60
		# print "startTime = " + str(startTime) + ", endTime = " + str(endTime) + ", elapsedTime = " + str(elapsedTime)
		sys.stderr.write("Elapsed Time: " + str(hours) + "hrs " + str(mins) + "mins " + str(secs) + "secs " + str(ms) + "ms; ")
		sys.stderr.write("Ending Time: " + time.strftime('%X %x %Z') + "\n")
		
			
	# Print Elapsed time, given start time
	def time(startTime):
		endTime = time.time()
		elapsedTime = endTime - startTime
		secs = int(elapsedTime)
		ms = str(elapsedTime - secs)[2:6]
		mins = secs / 60
		secs = secs % 60
		hours = mins / 60
		mins = mins % 60
		# print "startTime = " + str(startTime) + ", endTime = " + str(endTime) + ", elapsedTime = " + str(elapsedTime)
		sys.stdout.write("Elapsed Time: " + str(hours) + "hrs " + str(mins) + "mins " + str(secs) + "secs " + str(ms) + "ms; ")
		sys.stdout.write("Ending Time: " + time.strftime('%X %x %Z') + "\n")

	# Print status message about a file name
	def pstatus(fileName):
                sys.stderr.write("Processing " + fileName + "... ")

	# Print Elapsed time, given start time
	def getElapsedTime(startTime):
		endTime = time.time()
		elapsedTime = endTime - startTime
		secs = int(elapsedTime)
		ms = str(elapsedTime - secs)[2:6]
		mins = secs / 60
		secs = secs % 60
		hours = mins / 60
		mins = mins % 60
		# print "startTime = " + str(startTime) + ", endTime = " + str(endTime) + ", elapsedTime = " + str(elapsedTime)
		retstr = ""
		retstr += "Elapsed Time: " + str(hours) + "hrs " + str(mins) + "mins " + str(secs) + "secs " + str(ms) + "ms; "
		retstr += "Ending Time: " + time.strftime('%X %x %Z') + "\n"
		return retstr
		

	# Print Shell Command
	def printCmd(cmd, options=""):
		if (options.startswith("date")):
			print "date"
		#print "echo \"" + cmd + "\""
		print cmd
		if (options.endswith("newline")):
			print "echo\n"


	# Execute Shell Command
	def executeCmd(cmd, options=""):
		if (options.startswith("date")):
			os.system("date")
		if (options.find("noprint") == -1):
			#os.system("echo \"" + cmd + "\"")
			print cmd
		os.system(cmd)
		if (options.endswith("newline")):
			os.system("echo\n")

	def checkDir(dir, options=""):
		if (not os.path.exists(dir)):
			utils.executeCmd("mkdir " + dir, options)

	def getNumLines(fileName):
		s = os.popen("wc -l " + fileName).readlines()
		if (len(s) == 0):
			print fileName,s
			return 0
		return int(split(s[0].strip(), ' ')[0])

	def getCmdOutput(cmd):
		s = os.popen(cmd).readlines()
		if (len(s) == 0):
			print cmd,s
			return '0'
		return split(s[0].strip(), ' ')[0]

	# Sort the hashtable by its values (in decreasing order)
	def sortByValues(hash):
		items = hash.items();
		items = [(v,k) for (k, v) in items]	# invert keys to values
		items.sort()
		items.reverse()	# so largest is first
		items = [(k, v) for (v, k) in items]	# map back keys to values
		return items

	# print the hashtable with sorted keys
	def printBySortedKeys(hash):
		items = utils.sortByValues(hash)
		keys = hash.keys()
		keys.sort()
		#print keys
		for k in keys:
			print str(k) + ':' + str(hash[k]) + ',',
		print ""

	def writeHashItemsToFile(hashItems, fileName, header):
		fp = open(fileName, 'w')
		fp.write(header + "\n")
		for item in freqItems:
			fp.write(item[0] + "\t" + str(item[1]) + "\n")
		fp.close()

	def readFileToHash(fileName):
		hash = {}
		fp = open(fileName, 'r')
		while 1:
			line = lower(fp.readline().strip())
			if not line: break
			hash[line] = "1"

		fp.close()
		return hash

	def readFileToArray(fileName):
		arr = []
		fp = open(fileName, 'r')
		while 1:
			line = lower(fp.readline().strip())
			if not line: break
			arr.append(line)

		fp.close()
		return arr


	# Return a dictionary (hash table) with key as the feature name
	# and value as an array or list of its feature values
	def getFeatureGrid(featureFile, excludeFeatures):
		featGrid = {}
		categoricalFeats = {}

		fp = open(featureFile, 'r')
		featNames = split(fp.readline().strip(), ',')
		count = 0
		while 1:
			line = fp.readline().strip()
			if not line:
				break
			feats = split(line, ',')
			#if (count % 10000 == 0 and count != 0):
				#print "count = " + str(count)
				#break
			for i in range(len(feats)):
				if (featNames[i].startswith("prob_") or
					featNames[i].startswith("fprob") or
					featNames[i].startswith("fsmu") or
					featNames[i].startswith("fscu") or
					featNames[i].startswith("fqtf") or
					featNames[i].startswith("fctf") or
				 	featNames[i].startswith("weight") or
					excludeFeatures.has_key(featNames[i])):
					if (not categoricalFeats.has_key(featNames[i])):
						categoricalFeats[featNames[i]] = []
					categoricalFeats[featNames[i]].append(float(feats[i]))
				else:
					if (not featGrid.has_key(featNames[i])):
						featGrid[featNames[i]] = []
					featGrid[featNames[i]].append(float(feats[i]))
			count += 1

		#print featGrid
		#print categoricalFeats
		return (featGrid, categoricalFeats)


	# Return a dictionary (hash table) with key as the feature name
	# and value as an array or list of its feature values
	def getRegularFeatureGrid(featureFile):
		featGrid = {}

		startTime = time.time()
		fp = open(featureFile, 'r')
		featNames = split(fp.readline().strip(), ',')
		count = 0
		while 1:
			line = fp.readline().strip()
			if not line:
				break
			feats = split(line, ',')
			for i in range(len(feats)):
				if (not featGrid.has_key(featNames[i])):
					featGrid[featNames[i]] = []
				featGrid[featNames[i]].append(float(feats[i]))
			count += 1
			if (count % 5000 == 0):
				print "Processed " + str(count) + " lines..."
				utils.printElapsedTime(startTime)

		return featGrid


	# Take all the categorical features and continuous features
	# (either with or without normalization)
	# along with their values, and write them out to a file
	def writeFeatGrid(featGrid, outputFile):
		# combine all the features and sort them
		allFeats = featGrid.keys()
		allFeats.sort()

		# Write feature names
		fp = open(outputFile, 'w')
		gradePresent = 0
		outStr = ""
		for i in range(len(allFeats)):
			featName = allFeats[i]
			if featName == "grade":
				gradePresent = 1
				continue
			outStr += (featName + ",")
		if (gradePresent == 1):
			fp.write(outStr + "grade\n")
		else:
			fp.write(outStr[:-1] + "\n")

		# Write feature values
		numExamples = len(featGrid[allFeats[0]])
		for i in range(numExamples):
			outStr = ""
			for featName in allFeats:
				#print "{"+featName+"}"
				if (featName == "grade"):
					continue
				outStr += (str(featGrid[featName][i]) + ",")
			if (gradePresent == 1):
				try:
					fp.write(outStr + str(featGrid["grade"][i]) + "\n")
				except IndexError:
					print "index error!!!"
					print featName
					fp.write(outStr + "-1\n")
					
			else:
				fp.write(outStr[:-1] + "\n")

			#break
		
		fp.close()

	def sortCsvFeatures(csvFile):
		fp = open(csvFile, 'r')
		featNames = split(fp.readline().strip(), ',')
		featNames = featNames[:-1]
		sortedFeatNames = []
		for feat in featNames:
			sortedFeatNames.append(feat)
		sortedFeatNames.sort()
		featNames.append("grade")
		sortedFeatNames.append("grade")
		sortedInds = {}
		outStr = ''
		for i in range(len(sortedFeatNames)):
			sortedInds[i] = featNames.index(sortedFeatNames[i])
			outStr += (sortedFeatNames[i]+',')
		print outStr[:-1]

		while 1:
			line = fp.readline().strip()
			if not line: break
			vals = split(line, ',')
			outStr = ''
			for i in range(len(vals)):
				outStr += (vals[sortedInds[i]]+',')
			print outStr[:-1]
		
		fp.close()
		


	# compute dcg by testing it using mlr model
	def computeDCG5(featureFile, quFile, judgedFile, mlrCFile, tempDir):
		dcgFile = tempDir + "/" + featureFile[featureFile.rfind("/")+1:] + ".dcgTMP"
		if (os.path.exists(dcgFile)):
			os.system("rm " + dcgFile)
		cmd = "perl /home/svadrevu/bin/mytools/mlrCfile2DCG.pl " + mlrCFile + " " + featureFile + " " + quFile + " " + judgedFile + " " + tempDir + " > " + dcgFile
		print cmd
		os.system(cmd)
		(dcg1, dcg5) = parseDCGFile(dcgFile)
		return dcg5


	def containsAlpha(currStr):
		for i in range(len(currStr)):
			if ((currStr[i] >= 'a' and currStr[i] <= 'z') or
				(currStr[i] >= 'A' and currStr[i] <= 'Z')):
				return True
		return False

	def containsAlphaNumeric(currStr):
		for i in range(len(currStr)):
			if ((currStr[i] >= 'a' and currStr[i] <= 'z') or
				(currStr[i] >= 'A' and currStr[i] <= 'Z') or
				(currStr[i] >= '0' and currStr[i] <= '9')):
				return True
		return False

	def containsOnlyNumbers(currStr):
		for i in range(len(currStr)):
			if (not ((currStr[i] >= '0' and currStr[i] <= '9') or
 				currStr[i] == ',' or currStr[i] == '.') or currStr[i] == ' '):
				return False
		return True

	def stripNonAlphaNumeric(currStr):
		outStr = ''
		for i in range(len(currStr)):
			if ((currStr[i] >= 'a' and currStr[i] <= 'z') or
				(currStr[i] >= 'A' and currStr[i] <= 'Z') or
				(currStr[i] >= '0' and currStr[i] <= '9') or
				currStr[i] == ' '):
				outStr += currStr[i]
		return outStr


	def containsSpecialSymbols(currStr):
		if (currStr.find("!") != -1 or
			currStr.find("@") != -1 or
			currStr.find("#") != -1 or
			currStr.find("$") != -1 or
			currStr.find("%") != -1 or
			currStr.find("^") != -1 or
			currStr.find("&") != -1 or
			currStr.find("*") != -1 or
			currStr.find("(") != -1 or
			currStr.find(")") != -1 or
			currStr.find("[") != -1 or
			currStr.find("]") != -1 or
			currStr.find("+") != -1 or
			currStr.find("-") != -1 or
			currStr.find("`") != -1 or
			currStr.find("~") != -1 or
			currStr.find(":") != -1 or
			currStr.find(";") != -1):
			return True
		return False

	# parse the dcg file and get DCG5
	def parseDCGFile(dcgFile):
		fp = open(dcgFile, 'r')
		dcg1 = 0.0
		dcg5 = 0.0
		overalldcg1 = 0.0
		overalldcg5 = 0.0
		numQueries = 0

		header = split(fp.readline().strip(), '\t')
		findex = {}
		for i in range(len(header)):
			findex[header[i]] = i
		#print findex
		
		
		while 1:
			line = fp.readline()
			if not line: break
			line = line.strip()
			if (line.startswith("query\tdcg-1") or line == ""):
				continue
			if (line.startswith("overallDCG-1")):
				overalldcg1 = float(split(line, '\t')[1])
			elif (line.startswith("overallDCG-5")):
				overalldcg5 = float(split(line, '\t')[1])
			else:
				toks = split(line, '\t')
				dcg1 += float(toks[findex['dcg-1']])
				dcg5 += float(toks[findex['dcg-5']])
				#print toks[0],toks[findex['dcg-1']],toks[findex['dcg-5']]
				numQueries += 1

		if (overalldcg1 != 0.0 or overalldcg5 != 0.0):
			return (overalldcg1, overalldcg5)
		else:
			return (dcg1/float(numQueries), dcg5/float(numQueries))


	# Compare two search engines (dcg files)
	def compareEngines(dcgFile1, dcgFile2, evalFile, options="noheader"):
		if (os.path.exists(evalFile)):
			utils.executeCmd("rm " + evalFile, "noprint")
		utils.executeCmd("/home/svadrevu/bin/PIAnalysis/CompareEngines.py " + dcgFile1 + " " + dcgFile2 + " > " + evalFile, "noprint")

		# The case with no common queries, just skip this eval file
		numLines = utils.getNumLines(evalFile)
		if (numLines <= 4):
			print "No Common Queries, skipping DCG comparision for " + evalFile
			return (0.0, "Dummy")
		
		evals = utils.parseCompareEnginesOutputFile(evalFile)

		# Format the Output String
		evalStr = ""
		if (options == "header"):
			evalStr += "===========================================================================================================================================\n"
			evalStr += "Model".ljust(40) + "\t" + "Num_Queries\tNew_DCG5\tGain\tWilcoxon-Pval\tT-test-Pval\tNew_DCG1\tGain\tWilcoxon-Pval\tT-test-Pval\tDegraded_Queries\tImproved_Queries\tImprovement_Gain"
			evalStr += "===========================================================================================================================================\n"

		dcg5 = float(evals[1])

		dcg1Stem = dcgFile1[dcgFile1.rfind("/")+1:-4]
		dcg2Stem = dcgFile2[dcgFile2.rfind("/")+1:-4] 
		evalStr += dcg2Stem.ljust(40) + "\t"
		#evalStr += (dcg1Stem+'#'+dcg2Stem).ljust(80) + "\t"
		for val in evals[0:14]:
			evalStr += (val + "\t")
		evalStr + "\n"
		return (dcg5,evalStr)

	def parseCompareEngines(dcgFile1, dcgFile2):
		pfp = os.popen("/home/svadrevu/bin/PIAnalysis/CompareEngines.py " + dcgFile1 + " " + dcgFile2 + " -d1 -m1")
		try:
			outArray = utils.parseCompareEnginesOutputFP(pfp)
		except:
			print dcgFile1
			print dcgFile2
			raise
		return outArray
		

			
	def parseCompareEnginesOutputFile(compareEnginesOutputFile):
		pfp = open(compareEnginesOutputFile, "r")
		outArray = utils.parseCompareEnginesOutputFP(pfp)
		pfp.close()
		return outArray

	
	def parseCompareEnginesOutputFP(outputFP):
		outArray = []
		metrics = {}
		for line in outputFP:
			if (line.startswith("Base") or
				line.startswith("New") or
				line.startswith("Metric")):
				continue
			
			# Number of common queries
			if line.strip().endswith("queries):"):
				tmp = line[line.find("(")+1:]
				metrics['queries'] = tmp[:tmp.find(" ")]
				#outArray.append(tmp[:tmp.find(" ")])

			# Improved Queries
			if (line.startswith("ImprovedQrys")):
				#ImprovedQrys\t%s\t\t%s\t\t%f
				toks = split(line.strip(), '\t')
				#print toks
				metrics['ImprovedQrys'] = [toks[1],toks[3],toks[5]]
				#outArray.append(toks[1])
				#outArray.append(toks[3])
				#outArray.append(toks[5])

			elif (line.startswith("PCT_PAT1")):
				toks = split(line.strip(), '\t')
				#print toks
				metrics[toks[0]] = toks[2:6]
				#for tok in toks[2:6]:
				#	outArray.append(tok)
				

			# DCG-5 & DCG-1
			#if (line.startswith("DCG-5") or line.startswith("DCG-1")):
			else:
				#DCG-5\t\t%f\t%f\t%f
				toks = split(line.strip(), '\t')
				#print toks
				metrics[toks[0]] = toks[2:6]
				#for tok in toks[3:7]:
				#	outArray.append(tok)


		#print metrics
		if (not metrics.has_key('queries')):
			return outArray
		outArray.append(metrics['queries'])
		for tok in metrics['CDCG']:
			outArray.append(tok)
		for tok in metrics['DCG-5']:
			outArray.append(tok)
		for tok in metrics['DCG-1']:
			outArray.append(tok)
		for tok in metrics['NDCG']:
			outArray.append(tok)
		#for tok in metrics['ImprovedQrys']:
		#	outArray.append(tok)
				
		return outArray
		


	# Compare two dcg files and return the difference
	# Extend to print latex and csv strings too...
	def compareDcgFiles(dcgFile1, dcgFile2, evalFile, options="noheader"):
		if (os.path.exists(evalFile)):
			utils.executeCmd("rm " + evalFile, "noprint")
		utils.executeCmd("wilcoxon.newversion.py " + dcgFile1 + " " + dcgFile2 + " > " + evalFile, "noprint")

		# The case with no common queries, just skip this eval file
		numLines = utils.getNumLines(evalFile)
		if (numLines <= 1):
			print "No Common Queries, skipping DCG comparision for " + evalFile
			return (0.0, "Dummy")
		
		
		evals = utils.parseWilcoxonOutput(evalFile)

		# Format the Output String
		evalStr = ""
		if (options == "header"):
			evalStr += "===========================================================================================================================================\n"
			evalStr += "Model".ljust(60) + "\t" + "ODCG5".ljust(10) + "NDCG5".ljust(10) + "Gain".ljust(10) + "Pval".ljust(10) + "ODCG1".ljust(10) + "NDCG1".ljust(10) + "Gain".ljust(10) + "Pval".ljust(10) + "\n"
			evalStr += "===========================================================================================================================================\n"

		gain = float(evals[2])

		dcg1Stem = dcgFile1[dcgFile1.rfind("/")+1:-4]
		dcg2Stem = dcgFile2[dcgFile2.rfind("/")+1:-4] 
		evalStr += dcg2Stem.ljust(60) + "\t"
		#evalStr += (dcg1Stem+'#'+dcg2Stem).ljust(80) + "\t"
		for val in evals[0:8]:
			evalStr += (str(round(float(val), 4)).ljust(10))
		evalStr + "\n"
		return (gain,evalStr)


	# Parse wilcoxon output and return the dcg1 gains and dcg5 gains
	def parseWilcoxonOutput(wilcoxonOutputFile):
		#print "Parsing " + wilcoxonOutputFile
		outArray = []
		pfp = open(wilcoxonOutputFile, "r")

		line = pfp.readline()
		if (line.startswith("there are")):
			line = pfp.readline()
		tvals = line.split(" ")
		outArray.append(tvals[2])
		outArray.append(tvals[3])
		outArray.append(tvals[-1][:-1])

		line = pfp.readline()
		pvals = line.split(" ")
		if (pvals[-1][:-1] == "None"):
			outArray.append("0.0")
		else:
			outArray.append(pvals[-1][:-1])

		line = pfp.readline();
		line = pfp.readline();
		line = pfp.readline();
		tvals = line.split(" ")
		outArray.append(tvals[2])
		outArray.append(tvals[3])
		outArray.append(tvals[-1][:-1])

		line = pfp.readline()
		pvals = line.split(" ")
		if (pvals[-1][:-1] == "None"):
			outArray.append("0.0")
		else:
			outArray.append(pvals[-1][:-1])		
		pfp.close()
		return outArray


    # whether the query is a domain query
	def checkDomainQuery(q):
		dotCount = anuCount = 0
		for c in q:
			if c == '.': dotCount += 1
			elif ('a' <= c and c <= 'z') or \
				('A' <= c and c <= 'Z') or \
				('0' <= c and c <= '9'):
				anuCount += 1
			else: return False
		return (((dotCount > 0) and (anuCount > dotCount + 2)) or
				q.startswith("http://"))


	def computeKLDivergence(pdf1, pdf2):
		kld = 0.0
		for i in range(len(pdf1)):
			if (pdf1[i] != 0 and pdf2[i] != 0):
				kld += pdf1[i] * math.log(pdf1[i]/pdf2[i], 2)

		return -kld


	def computeJDivergence(pdf1, pdf2):
		symmetricKLD = 0
		for i in range(len(pdf1)):
			if (pdf1[i] != 0 and pdf2[i] != 0):
				symmetricKLD += (pdf1[i] - pdf2[i]) * math.log(pdf1[i]/pdf2[i])

		symmetricKLD = 1 - math.pow(2, -symmetricKLD)
		return symmetricKLD


	# Compute Probability Distribution (Probability Density Function)
	# for a given population, by binning the values into specified number
	# of buckets
	def computePDF(inputVals, numBins):

		#print inputVals

		binFreqs = []
		for i in range(numBins):
			binFreqs.append(0)

		min = 100000000
		max = -100000000
		mean = 0

		for val in inputVals:
			if (val < min):
				min = val
			if (val > max):
				max = val
			mean = mean+val

		interval = (max-min)/numBins
		mean /= len(inputVals)

		#print "min = " + str(min) + ", max = " + str(max) + ", mean = " + str(mean) + ", interval = " + str(interval)

		# Compute the partitions between each of the intervals
		partitions = []
		partitions.append(min)
		count = 1
		while (count <= numBins+1):
			partitions.append(partitions[count-1]+interval)
			count += 1
		partitions[numBins+1] += interval  # for boundary conditions at end

		#print partitions
		for val in inputVals:
			for i in range(numBins):
				if (val >= partitions[i] and val < partitions[i+1]):
					binFreqs[i] += 1

		#print binFreqs
		pdf = []
		for val in binFreqs:
			pdf.append(float(val)/float(len(inputVals)))

		#print pdf
		return pdf

	def canonicalize(url):
		url = re.sub("\s+", " ", url)
		url = re.sub("^\s+", "", url)
		url = re.sub("\s+$", "", url)
		url = lower(url)
		
		#url = re.sub("^http\://", "", url)
		#url = re.sub("^https\://", "", url)
		#url = re.sub("^www\.", "", url)
		url = re.sub("\/$", "", url)

		url = re.sub("index.php$", "", url)
		url = re.sub("index.html$", "", url)
		url = re.sub("index.asp$", "", url)
		url = re.sub("index.aspx$", "", url)
		url = re.sub("index.jsp$", "", url)
		url = re.sub("index.cfm$", "", url)
		url = re.sub("index.shtml$", "", url)
		url = re.sub("index.jhtml$", "", url)
		url = re.sub("index.htm$", "", url)
		url = re.sub("index.cgi$", "", url)
		url = re.sub("main.php$", "", url)
		url = re.sub("main.html$", "", url)
		url = re.sub("main.asp$", "", url)
		url = re.sub("main.aspx$", "", url)
		url = re.sub("main.jsp$", "", url)
		url = re.sub("main.cfm$", "", url)
		url = re.sub("main.shtml$", "", url)
		url = re.sub("main.jhtml$", "", url)
		url = re.sub("main.htm$", "", url)
		url = re.sub("main.cgi$", "", url)
		url = re.sub("default.php$", "", url)
		url = re.sub("default.html$", "", url)
		url = re.sub("default.asp$", "", url)
		url = re.sub("default.aspx$", "", url)
		url = re.sub("default.jsp$", "", url)
		url = re.sub("default.cfm$", "", url)
		url = re.sub("default.shtml$", "", url)
		url = re.sub("default.jhtml$", "", url)
		url = re.sub("default.htm$", "", url)
		url = re.sub("default.cgi$", "", url)

		return url


	def canonicalizeComplete(url):
		url = re.sub("\s+", " ", url)
		url = re.sub("^\s+", "", url)
		url = re.sub("\s+$", "", url)
		url = lower(url)
		
		#url = re.sub("^http\://", "", url)
		#url = re.sub("^https\://", "", url)
		#url = re.sub("^www\.", "", url)
		url = re.sub("\/$", "", url)

		url = re.sub("index.php$", "", url)
		url = re.sub("index.html$", "", url)
		url = re.sub("index.asp$", "", url)
		url = re.sub("index.aspx$", "", url)
		url = re.sub("index.jsp$", "", url)
		url = re.sub("index.cfm$", "", url)
		url = re.sub("index.shtml$", "", url)
		url = re.sub("index.jhtml$", "", url)
		url = re.sub("index.htm$", "", url)
		url = re.sub("index.cgi$", "", url)
		url = re.sub("main.php$", "", url)
		url = re.sub("main.html$", "", url)
		url = re.sub("main.asp$", "", url)
		url = re.sub("main.aspx$", "", url)
		url = re.sub("main.jsp$", "", url)
		url = re.sub("main.cfm$", "", url)
		url = re.sub("main.shtml$", "", url)
		url = re.sub("main.jhtml$", "", url)
		url = re.sub("main.htm$", "", url)
		url = re.sub("main.cgi$", "", url)
		url = re.sub("default.php$", "", url)
		url = re.sub("default.html$", "", url)
		url = re.sub("default.asp$", "", url)
		url = re.sub("default.aspx$", "", url)
		url = re.sub("default.jsp$", "", url)
		url = re.sub("default.cfm$", "", url)
		url = re.sub("default.shtml$", "", url)
		url = re.sub("default.jhtml$", "", url)
		url = re.sub("default.htm$", "", url)
		url = re.sub("default.cgi$", "", url)

		return url


	def normalizeQry(qry):
		try:
			return os.popen("echo " + qry + " | /net/irdev20/export/crawlspace/yazhang/switch/bin/query_parser.sh").readlines()[0]
		except:
			return qry

	def generateNGrams(currStr, ngramLen, filter):
		allNGrams = []

		toks = re.split(' |\.', currStr)
		#print currStr,toks
		for n in range(ngramLen):
			n += 1  # n = length of the ngram
			#print "n = " + str(n)
			for i in range(len(toks)):
				#print "i = " + str(i)
				currNGram = ""
				if (i+n <= len(toks)):
					for j in range(n):
						j += i
						if ((filter == 1) and
							(not utils.containsAlphaNumeric(toks[j]))):
							continue
						#print "j = " + str(j)
						currNGram = (currNGram + toks[j] + " ")
					currNGram = currNGram[:-1]
					allNGrams.append(currNGram)
		allNGrams.reverse()
		return allNGrams


	def generateNGramsIndex(currStr, ngramLen, filter, includeIndex, startIndex):
		allNGrams = []

		toks = re.split(' |\.', currStr)
		#print currStr,toks
		for n in range(ngramLen):
			n += 1  # n = length of the ngram
			#print "n = " + str(n)
			for i in range(len(toks)):
				#print "i = " + str(i)
				currNGram = ""
				if (i+n <= len(toks)):
					for j in range(n):
						j += i
						if ((filter == 1) and
							(not utils.containsAlphaNumeric(toks[j]))):
							continue
						#print "j = " + str(j)
						if (includeIndex and
						    currNGram == ""):
							currNGram = str(startIndex+j)+"||"
						currNGram = (currNGram + toks[j] + " ")
					currNGram = currNGram[:-1]
					allNGrams.append(currNGram)
		allNGrams.reverse()
		return allNGrams


	def parseNGrams(str, ngramLen, startIndex):
		phoneRE = re.compile("\(?[1-9]\d{2}\)?\s?[-\.]?\d{3}[-\.]?\d{4}")
		emailRE = re.compile("\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b")
		zipcodeRE = re.compile("\b\d{5}\b|\b\d{5}-\d{4}\b")
		timeRE = re.compile("(?:0?[1-9]:[0-5]|1(?=[012])\d:[0-5])\d(?:[ap]m)?")
		stopwords = split('1,2,3,4,5,a,about,all,also,an,and,any,are,as,at,be,been,but,by,can,com,de,do,find,for,from,get,go,has,have,here,how,i,if,in,info,into,is,it,its,like,me,my,of,on,or,our,out,re,s,so,some,than,that,the,their,there,these,they,this,through,to,up,use,was,we,web,what,when,which,who,with,would,www,you,your,0,each,he,his,should,such,01,after,am,another,area,around,based,between,come,comes,could,does,don,during,end,even,every,form,group,had,http,including,it,made,main,must,order,privacy,said,same,say,since,sites,those,three,updated,very,were,while,will,without', ',')
		swRE = utils.buildREPattern(stopwords)

		str = swRE.sub('', str).replace(")", "").replace("(", "").strip()

		# str, ngramlen, filterNonAlphaChars, includeIndex, startIndex
		toks = utils.generateNGramsIndex(str, ngramLen, 1, True, startIndex)
		#print toks
		ret = []
		for tok in toks:
			#if (not utils.containsAlphaNumeric(tok)):
			#	continue
			if (tok == ""):
				continue
			#print tok
			try:
				(index, ngram) = split(tok, '||')
			except:
				print tok
				raise
	
			if (phoneRE.match(ngram)):
				ngram = "PHONENUM"
			if (emailRE.match(ngram)):
				ngram = "EMAIL"
			if (timeRE.match(ngram)):
				ngram = "TIME"
			if (zipcodeRE.match(ngram)):
				ngram = "ZIPCODE"

			if (len(ngram) <= 1):
				continue
			if (utils.containsOnlyNumbers(ngram)):
				ngram = "NUMBER"
			if (ngram[0] == "$" and utils.containsOnlyNumbers(ngram[1:])):
				ngram = "PRICE"
			#ngram = "{"+ngram+"}"
			ret.append(ngram+"||"+index)

		return ret
		


	def trainTestModel(trainCsvFile, testCsvFile, testQUFile, testJudgedFile, destModelName, subDirName, options="train-test"):

		modelDir = "/home/svadrevu/TreeNet-in/models/" + subDirName + "/" + destModelName 
		cFile = modelDir + "/models/" + destModelName + ".c"
		dcgFile = modelDir + "/models/" + destModelName + ".dcg"

		# Training
		if (options.startswith("train")):
			utils.executeCmd("csh /home/svadrevu/bin/mytools/GenerateTreenetCommand.csh " + subDirName + " " + destModelName + " " + trainCsvFile)
			utils.executeCmd("sh " + modelDir + "/bin/TreeNetCommand.sh")
	
		# Testing
		if (options.endswith("test")):
			cFile = modelDir + "/models/" + destModelName + ".c"
			dcgFile = modelDir + "/models/" + destModelName + ".dcg"
			#origCFile = cFile + ".orig"
			#utils.executeCmd("mv " + cFile + " " + origCFile)
			#utils.executeCmd("sed \'s/.z/0/g\' " + origCFile + " > " + cFile, "newline")

			if (os.path.exists(dcgFile)):
				utils.executeCmd("rm " + dcgFile)
			tempDir = modelDir + "/temp"
			#utils.executeCmd("cd " + modelDir + "/temp/")
			utils.executeCmd("rm " + tempDir + "/*")
			utils.executeCmd("perl /home/svadrevu/bin/mytools/Cfile2DCG.pl "
							 + " " + cFile + " " + testCsvFile + " " + testQUFile
							 + " " + testJudgedFile + " " + tempDir
							 + " > " + dcgFile, "date-newline")
			(dcg1, dcg5) = utils.parseDCGFile(dcgFile)
			return (dcg5, dcgFile)

		return (-1, dcgFile)

	def testModel(testCsvFile, testQUFile, testJudgedFile, destModelName, subDirName, testSuffix):

		modelDir = "/home/svadrevu/TreeNet-in/models/" + subDirName + "/" + destModelName 
		cFile = modelDir + "/models/" + destModelName + ".c"
		dcgFile = modelDir + "/models/" + destModelName + "-" + testSuffix + ".dcg"

		if (os.path.exists(dcgFile)):
			utils.executeCmd("rm " + dcgFile)
	   	tempDir = modelDir + "/temp"
		#utils.executeCmd("cd " + modelDir + "/temp/")
		utils.executeCmd("rm " + modelDir + "/temp/*")
		utils.executeCmd("perl /home/svadrevu/bin/mytools/Cfile2DCG.pl "
						 + " " + cFile + " " + testCsvFile
						 + " " + testQUFile + " " + testJudgedFile
						 + " " + tempDir + " > " + dcgFile, "date-newline")
		(dcg1, dcg5) = utils.parseDCGFile(dcgFile)
		return (dcg5, dcgFile)


	def buildREPattern(arr):
		rePattern = '|'.join(map(re.escape, arr))
		rePattern = r"\b(" + rePattern + r")\b"
		return re.compile(rePattern)

	def deleteFile(fileName, options=""):
		if (os.path.exists(fileName)):
			utils.executeCmd("rm " + fileName, options)
		return fileName

	# Given csv, qu and judged files, compute the dcg file
	# with all the temporary rank and response files stored in temp dir
	def computeMlrDCGFile(mlrCFile, csvFile, quFile, judgeFile, dcgFile, tempDir, invertFlag, options=""):
		binDir = "/home/svadrevu/cvscode/search/i18nrel/PI_CV"
		if (tempDir.endswith("/")):
			tempDir = tempDir[:-1]

		sortMethod = binDir + "/SortScoreA"
		if (invertFlag == "1"):
			sortMethod = binDir + "/SortScoreD"

		dcgFile = utils.deleteFile(dcgFile, options)
		responseFile = utils.deleteFile(tempDir + "/tmp.response", options)
		rankFileUnsorted = utils.deleteFile(tempDir + "/tmp.rank.unsorted", options)
		rankFileSorted = utils.deleteFile(tempDir + "/tmp.rank.sort", options)
		rankFileUniq = utils.deleteFile(tempDir + "/tmp.rank.uniq", options)

		utils.executeCmd(binDir + "/mlrcfile2normresp " + mlrCFile + " " + csvFile + " 0 > " + responseFile, options)
		utils.executeCmd("paste " + quFile + " " + responseFile + " > " + rankFileUnsorted, options)
		utils.executeCmd("cat " + rankFileUnsorted + " | " + sortMethod + " | " + binDir + "/FilterDomainQueries > " + rankFileSorted, options)
		utils.executeCmd("cat " + rankFileSorted + " | " + binDir + "/uniq-dedup > " + rankFileUniq, options)
		utils.executeCmd(binDir + "/computeDCG " + judgeFile + " " + rankFileUniq + " > " + dcgFile, options)


	# Extract top features from an MLR Dat File
	def extractTopFeatures(mlrDatFile):
		fp = open(mlrDatFile, 'r')
		retFeatures = []
		varImpFlag = 0
		for line in fp:
			line = line.strip()
			if not line: continue
			if (line.startswith("Variable Importance")):
				varImpFlag = 1
			if (varImpFlag == 1 and (line.startswith("Learn Sample") or line.startswith("There are"))):
				varImpFlag = 0
				break
			if (varImpFlag == 1 and line.startswith("F")):
				#print line
				toks = split(line, ' ')
				newtoks = []
				for tok in toks:
					if (tok): newtoks.append(tok)
				retFeatures.append((lower(newtoks[0]), newtoks[2]))
		return retFeatures
			
	def splitDataTrainTest(csv, qu, judged, targetDir):
		csvTr = os.path.join(targetDir, "tr.0.csv")
		quTr = os.path.join(targetDir, "tr.0.qu")
		judgedTr = os.path.join(targetDir, "tr.0.judged")

		csvTe = os.path.join(targetDir, "te.0.csv")
		quTe = os.path.join(targetDir, "te.0.qu")
		judgedTe = os.path.join(targetDir, "te.0.judged")

		if ((not os.path.exists(csvTr)) or
			(not os.path.exists(quTr)) or
			(not os.path.exists(judgedTr)) or
			(not os.path.exists(csvTe)) or
			(not os.path.exists(quTe)) or
			(not os.path.exists(judgedTe))):
			utils.executeCmd("python /home/svadrevu/bin/TreeNet/SplitDataIntoFolds.py " + csv + " " + qu + " " + judged + " " + targetDir + " 5")

		return (csvTr, quTr, judgedTr, csvTe, quTe, judgedTe)
		
	# Rotate a csv file (horizontal to vertical)
	def rotateFile(inFile, outFile, delimiter):
		#print "inFile = " + inFile
		fp = open(inFile, 'r')

		fileArr = []
		for line in fp:
			fileArr.append(split(line.strip(), delimiter))
		fp.close()

		fp = open(outFile, 'w')
		for i in range(len(fileArr[0])):
			fp.write(fileArr[0][i])
			for lineArr in fileArr[1:]:
				try:
					fp.write(delimiter + lineArr[i])
				except:
					print lineArr
					print "i = " + str(i) + ", total num of columns = " + str(len(lineArr[0]))
					raise
					sys.exit(1)
			fp.write("\n")
		fp.close()
		print "Rotated file from " + inFile + " to " + outFile


	# Rotate a csv file (horizontal to vertical)
	def rotateCsvFileWithFeats(inFile, outFile, delimiter, feats):
		fp = open(inFile, 'r')

		fileArr = []
		for line in fp:
			fileArr.append(split(line.strip(), delimiter))
		fp.close()

		fp = open(outFile, 'w')
		for i in range(len(fileArr[0])):
			if (not feats.has_key(fileArr[0][i])):
				continue
			fp.write(fileArr[0][i])
			for lineArr in fileArr[1:]:
				fp.write("," + lineArr[i])
			fp.write("\n")
		fp.close()

	# Rotate a csv file (horizontal to vertical)
	def rotateFile2(inFile, outFile, delimiter, startTime):
		fp = open(inFile, 'r')
		header = split(fp.readline(), delimiter)
		numCols = len(header)
		fp.close()
		fpw = open(outFile, 'w')
# 		maxThreads= 25

		i = 0
		while i < numCols:
			if (i % 1 == 100):
				print "processed " + str(i) + " features, curr feat = " + header[i]
				utils.printElapsedTime(startTime)

# 			rotateStruc = RotateStruct()
# 			rotateStruc.colID = i
			
# 			while (threading.activeCount() > maxThreads):
# 				print "Number of threads more than " + str(maxThreads) + ", sleeping...  " + time.strftime('%X %x %Z')
# 				time.sleep(3)
# 			rotThread = RotateThread(inFile, delimiter, rotateStruc)
# 			rotThread.start()

# 			print "rotateStr = " + rotateStruc.outputStr + ", colID = " + str(rotateStruc.colID)
# 			fpw.write(rotateStruc.outputStr[:-1] + '\n')
			
			currStr = ""
			fp = open(inFile, 'r')
			for line in fp:
				arr = split(line.strip(), delimiter)
				currStr += (arr[i] + delimiter)
			fp.close()
			fpw.write(currStr[:-1] + '\n')

			i += 1
		fpw.close()

			
	# Rotate a csv file (horizontal to vertical)
	def rotateCsvFileWithFeats2(inFile, outFile, delimiter, feats, startTime):
		fp = open(inFile, 'r')
		header = split(fp.readline(), delimiter)
		numCols = len(header)
		fp.close()
		fpw = open(outFile, 'w')

		i = 0
		while i < numCols:
			if (i % 100 == 0):
				print "processed " + str(i) + " features, curr feat = " + header[i]
				utils.printElapsedTime(startTime)

			currStr = ""
			count = 0
			fp = open(inFile, 'r')
			for line in fp:
				arr = split(line.strip(), delimiter)
				if (count == 0 and (not feats.has_key(arr[i]))):
					break
				currStr += (arr[i] + delimiter)
				count += 1
			fp.close()
			if (count != 0):
				fpw.write(currStr[:-1] + '\n')

			i += 1
		fpw.close()
			
	# Given two csv files, get the common features
	def getCommonFeats(csv1, csv2):
		csvfp1 = open(csv1, 'r')
		feats1 = split(csvfp1.readline().strip(), ',')
		csvfp2 = open(csv2, 'r')
		feats2 = split(csvfp2.readline().strip(), ',')
		commonfeats = {}
		for feat in feats1:
			present = 0
			for f in feats2:
				if (feat == f):
					present = 1
					break
			if (present == 1):
				commonfeats[feat] = "1"
		csvfp1.close()
		csvfp2.close()
		print "len(feats1) = " + str(len(feats1)) + ", len(feats2) = " + str(len(feats2)) + ", len(commonFeats) = " + str(len(commonfeats))
		return commonfeats

	# Returns the index of the fields
	def field_index(fields):
		index = {}
		for i in range(len(fields)):
			index[fields[i]] = i
		return index

	def binomCoeff(n, k):
		numerator = 1
		denominator = factorial(k)
		i = n
		while (i >= (n-k+1)):
			numerator *= i
			i -= 1
		
		return numerator/denominator
		
	def multinomCoeff(n, karr):
		numerator = factorial(n)
		denominator = 1
		for k in karr:
			denominator *= factorial(k)
			
		return numerator/denominator


	def logBinomCoeff(n, k):
		ret = 0.0

		i = n
		while (i >= (n-k+1)):
			ret += math.log(i, 2)
			i -= 1

		i = k
		while (i >= 1):
			ret -= math.log(i, 2)
			i -= 1
			
		return ret

	def logMultinomCoeff(n, karr):
		ret = 0.0

		i = n
		while (i >= 1):
			ret += math.log(i, 2)
			i -= 1

		for k in karr:
			i = k
			while (i >= 1):
				ret -= math.log(i, 2)
				i -= 1

		return ret

	def factorial(n):
		#print "Computing factorial for " + str(n)
		fact = 1
		i = 1
		while i <= n:
			fact *= i
			i += 1
		return fact


	def flatten(l):
		out = []
		for item in l:
			if isinstance(item, (list, tuple)):
				out.extend(flatten(item))
			else:
				out.append(item)
		return out

	def convertUnixTime(timeint):
		return datetime.datetime.fromtimestamp(timeint).strftime('%Y-%m-%d %H:%M:%S')


	def editDistance(s1, s2):
		l1 = len(s1)
		l2 = len(s2)
		
		matrix = [range(l1 + 1)] * (l2 + 1)
		for zz in range(l2 + 1):
			matrix[zz] = range(zz,zz + l1 + 1)
		for zz in range(0,l2):
			for sz in range(0,l1):
				if s1[sz] == s2[zz]:
					matrix[zz+1][sz+1] = min(matrix[zz+1][sz] + 1, matrix[zz][sz+1] + 1, matrix[zz][sz])
				else:
					matrix[zz+1][sz+1] = min(matrix[zz+1][sz] + 1, matrix[zz][sz+1] + 1, matrix[zz][sz] + 1)

		#print "That's the Levenshtein-Matrix:"
		#printMatrix(matrix)
		return matrix[l2][l1]


	def levenshtein_distance(first, second):
		"""Find the Levenshtein distance between two strings."""
		if len(first) > len(second):
			first, second = second, first
		if len(second) == 0:
			return len(first)
		first_length = len(first) + 1
		second_length = len(second) + 1
		distance_matrix = [[0] * second_length for x in range(first_length)]
		for i in range(first_length):
			distance_matrix[i][0] = i
			for j in range(second_length):
				distance_matrix[0][j]=j

		for i in xrange(1, first_length):
			for j in range(1, second_length):
				deletion = distance_matrix[i-1][j] + 1
				insertion = distance_matrix[i][j-1] + 1
				substitution = distance_matrix[i-1][j-1]
				if first[i-1] != second[j-1]:
					substitution += 1
				distance_matrix[i][j] = min(insertion, deletion, substitution)

		return distance_matrix[first_length-1][second_length-1]


	printCmd = staticmethod(printCmd)
	executeCmd = staticmethod(executeCmd)
	parseCmdLineArgs = staticmethod(parseCmdLineArgs)
	printElapsedTime = staticmethod(printElapsedTime)
	time = staticmethod(time)
	getElapsedTime = staticmethod(getElapsedTime)
	checkDir = staticmethod(checkDir)
	getNumLines = staticmethod(getNumLines)
	sortByValues = staticmethod(sortByValues)
	writeHashItemsToFile = staticmethod(writeHashItemsToFile)
	readFileToHash = staticmethod(readFileToHash)
	readFileToArray = staticmethod(readFileToArray)
	getFeatureGrid = staticmethod(getFeatureGrid)
	getRegularFeatureGrid = staticmethod(getRegularFeatureGrid)
	writeFeatGrid = staticmethod(writeFeatGrid)
	computeDCG5 = staticmethod(computeDCG5)
	compareDcgFiles = staticmethod(compareDcgFiles)
	compareEngines = staticmethod(compareEngines)
	containsAlpha = staticmethod(containsAlpha)
	containsAlphaNumeric = staticmethod(containsAlphaNumeric)
	containsOnlyNumbers = staticmethod(containsOnlyNumbers)
	stripNonAlphaNumeric = staticmethod(stripNonAlphaNumeric)
	parseDCGFile = staticmethod(parseDCGFile)
	parseWilcoxonOutput = staticmethod(parseWilcoxonOutput)
	parseCompareEnginesOutputFile = staticmethod(parseCompareEnginesOutputFile)
	parseCompareEnginesOutputFP = staticmethod(parseCompareEnginesOutputFP)
	parseCompareEngines = staticmethod(parseCompareEngines)
	computeKLDivergence = staticmethod(computeKLDivergence)
	computeJDivergence = staticmethod(computeJDivergence)
	computePDF = staticmethod(computePDF)
	canonicalize = staticmethod(canonicalize)
	canonicalizeComplete = staticmethod(canonicalizeComplete)
	generateNGrams = staticmethod(generateNGrams)
	parseNGrams = staticmethod(parseNGrams)
	trainTestModel = staticmethod(trainTestModel)
	testModel = staticmethod(testModel)
	checkDomainQuery = staticmethod(checkDomainQuery)
	buildREPattern = staticmethod(buildREPattern)
	deleteFile = staticmethod(deleteFile)
	computeMlrDCGFile = staticmethod(computeMlrDCGFile)
	extractTopFeatures = staticmethod(extractTopFeatures)
	sortCsvFeatures = staticmethod(sortCsvFeatures)
	normalizeQry = staticmethod(normalizeQry)
	splitDataTrainTest = staticmethod(splitDataTrainTest)
	containsSpecialSymbols = staticmethod(containsSpecialSymbols)
	rotateFile = staticmethod(rotateFile)
	rotateCsvFileWithFeats = staticmethod(rotateCsvFileWithFeats)
	rotateFile2 = staticmethod(rotateFile2)
	rotateCsvFileWithFeats2 = staticmethod(rotateCsvFileWithFeats2)
	field_index = staticmethod(field_index)
	binomCoeff = staticmethod(binomCoeff)
	multinomCoeff = staticmethod(multinomCoeff)
	logBinomCoeff = staticmethod(logBinomCoeff)
	logMultinomCoeff = staticmethod(logMultinomCoeff)
	printBySortedKeys = staticmethod(printBySortedKeys)
	generateNGramsIndex = staticmethod(generateNGramsIndex)
	flatten = staticmethod(flatten)
	convertUnixTime = staticmethod(convertUnixTime)
	editDistance = staticmethod(editDistance)
	levenshtein_distance = staticmethod(levenshtein_distance)
	getCmdOutput = staticmethod(getCmdOutput)
        pstatus = staticmethod(pstatus)
