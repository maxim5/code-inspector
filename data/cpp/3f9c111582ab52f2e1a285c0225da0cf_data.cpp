/*
 * data.cpp
 *
 *  Created on: Feb 4, 2010
 *      Author: wegmannd
 */
#include "data.h"
bool individual::addSNP(char Allele1, char Allele2, char refAllele1, char refAllele2){
	if(Allele1==refAllele1) allele1.push_back(1);
	else {
		if(Allele1==refAllele2) allele1.push_back(0);
		else {
			if(Allele1!='A' &&  Allele1!='T' && Allele1!='G' && Allele1!='C') allele1.push_back(9);
			else return false;
		}
	}
	if(Allele2==refAllele1) allele2.push_back(1);
	else {
		if(Allele2==refAllele2) allele2.push_back(0);
		else {
			if(Allele2!='A' &&  Allele2!='T' && Allele2!='G' && Allele2!='C') allele2.push_back(9);
			else return false;
		}
	}
	return true;
};
//---------------------------------------------------------------------------
int individual::getNumberMissingAlleles(){
	int m=0;
	std::vector<int>::iterator it;
	for(it=allele1.begin();it!=allele1.end();++it) if(*it==9) ++m;
	for(it=allele2.begin();it!=allele2.end();++it) if(*it==9) ++m;
	return m;
}
//---------------------------------------------------------------------------
int individual::getUnphasedGenotype(long snpId){
	int sum=allele1.at(snpId)+allele2.at(snpId);
	if(sum>2) return 9;
	else return sum;
}
//---------------------------------------------------------------------------
std::string individual::getEIGENSTRATphased(long snpId){
	return toString(allele1.at(snpId) +  allele2.at(snpId));
}
//---------------------------------------------------------------------------
std::string individual::getHAPMIXhaplotype1(long snpId){
	int temp=allele1.at(snpId);
	if(temp==9) return "?";
	else return toString(temp);
}
//---------------------------------------------------------------------------
std::string individual::getHAPMIXhaplotype2(long snpId){
	int temp=allele2.at(snpId);
	if(temp==9) return "?";
	else return toString(temp);
}
//---------------------------------------------------------------------------
std::string individual::getHAPMIXgenotype(long snpId){
	int sum=allele1.at(snpId)+allele2.at(snpId);
	if(sum>2) return "?";
	else return toString(sum);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
data::data(snpDB* MySnpDB, std::string filename, TLog* Logfile){
	//check extension to find appropriate method
	logfile = Logfile;
	nSNPs=0;
	nIndividuals=0;
	genotypingRate=0;
	mySnpDB=MySnpDB;
	std::string ext=readAfterLast(filename, '.');
	if(ext=="tped") readTPED(filename);
	else {
		if(ext=="phased") readPHASED(filename);
		else {
			if(ext=="ped") readPED(filename);
			else throw "Unknown file extension '"+ext+"'.";
		}
	}
}
//---------------------------------------------------------------------------
void data::readOneSNPPerLine(std::string filename, int nameColumn, int colsToSkip, bool header){
	//read a file with the following structure:
	// - one SNP per Line
	// - two cols per Individual
	// - first n cols contain no genotypes
	// - one of them contains the rsID
	clock_t start=clock();
	std::ifstream file;
	file.open(filename.c_str());
	if(!file)  throw "File '" + filename + " could not be read!";
	std::string tmp;
	int line=0;
	unsigned int firstLineLength=0;
	int nMissingSNP=0;
	long snpId=0;
	if(header) getline(file, tmp); // read header line
	snp* thisSNP;
	std::vector<std::string> val;
	while(file.good() && !file.eof()){
		++line;
		fillVectorFromLineWhiteSpaceSkipEmpty(file, val);
		if(val.size()>0){
			if(!firstLineLength){
				firstLineLength=val.size();
				nIndividuals=(firstLineLength-colsToSkip)/2;
				for(int i=0; i<nIndividuals;++i){
					myIndividuals.push_back(new individual());
				}
			} else {
				if(firstLineLength!=val.size()) throw "Wrong number of values in file '" + filename + " on line " + toString(line) + ".";
			}
			//check if SNP exists in DB, else ignore
			snpId=mySnpDB->getSnpId(val[nameColumn]);
			if(snpId>0){
				//insert SNP into list
				snps.insert(std::pair<long,long>(snpId, nSNPs));
				thisSNP=mySnpDB->getSNP(snpId);
				//insert alleles into individuals
				for(int i=0; i<nIndividuals;++i){
					if(!myIndividuals[i]->addSNP(val[colsToSkip+i*2].c_str()[0], val[colsToSkip+1+i*2].c_str()[0], thisSNP->allele1, thisSNP->allele2)){
						throw "Allele mismatch for SNP '"+ val[nameColumn] +"' in individual " + toString(i) + ": '" + val[colsToSkip+i*2].substr(0,1) + "/" + val[colsToSkip+1+i*2].substr(0,1) + "' instead of '" + thisSNP->getAllele1() + "' or '" +  thisSNP->getAllele2() + "'.";
					}
				}
				++nSNPs;
			} else ++nMissingSNP;
		}
	}
	file.close();
	logfile->write("done (" + toString( (clock() - start)/CLOCKS_PER_SEC ) + "s)!");
	logfile->conclude("Read " + toString(nIndividuals) + " individuals and " + toString(nSNPs) + " SNPs.");
	if(nMissingSNP>0) logfile->conclude(toString(nMissingSNP) + " SNPs are unknown and have been ignored!");
	genotypingRate = getGenotypingRate();
	if(nSNPs<1) throw "Error, no SNPs left!";
	logfile->conclude("Overall genotyping rate is " + toString(genotypingRate) + ".");
}
//---------------------------------------------------------------------------
void data::readTPED(std::string filename){
	//read a TPED file into the data DB
	logfile->listFlush("Reading data from tped file '" + filename + "' ...");
	readOneSNPPerLine(filename, 1, 4, false);
}
//---------------------------------------------------------------------------
void data::readPHASED(std::string filename){
	//read a TPED file into the data DB
	logfile->listFlush("Reading data from phased file '" + filename + "' ...");
	readOneSNPPerLine(filename, 0, 2, true);
}
//---------------------------------------------------------------------------
void data::readPED(std::string filename){
	logfile->startIndent("Reading data from ped file '" + filename + "':");
	clock_t start=clock();
	//read the map file first to get the SNPs
	std::string mapname=filename;
	mapname=extractBeforeLast(mapname, '.');
	mapname+=".map";
	logfile->listFlush("Reading the map file '" + mapname + "' ...");
	std::ifstream file;
	file.open(mapname.c_str());
	if(!file)  throw "map file '" + mapname + " could not be read!";

	//go through the map file
	long line=0;
	int nMissingSNP=0;
	std::vector<bool> SNPok;
	std::vector<long> snpIDs;
	long snpId;
	std::vector<std::string> val;
	while(file.good() && !file.eof()){
		++line;
		fillVectorFromLineWhiteSpaceSkipEmpty(file, val);
		if(val.size()>0){
			if(val.size()!=4) throw "Wrong number of values in map file '" + mapname + " on line " + toString(line) + ".";
			//check if SNP exists in DB, else ignore
			snpId=mySnpDB->getSnpId(val[1]);
			if(snpId>0){
				snps.insert(std::pair<long,long>(snpId, nSNPs));
				++nSNPs;
				SNPok.push_back(true);
				snpIDs.push_back(snpId);
			} else {
				++nMissingSNP;
				SNPok.push_back(false);
			}
		}
	}
	file.close();
	logfile->write("done (" + toString( (clock() - start)/CLOCKS_PER_SEC ) + "s)!");
	logfile->conclude("Read " + toString(nSNPs) + " SNPs.");
	if(nMissingSNP>0) logfile->conclude(toString(nMissingSNP) + " SNPs are unknown and have been ignored!");
	if(nSNPs<1) throw "Error, no SNPs left!";

	//reading ped file
	start=clock();
	logfile->listFlush("Reading the ped file '" + filename + "' ...");
	file.open(filename.c_str());
	if(!file)  throw "ped file '" + filename + " could not be read!";
	line=0;
	nIndividuals=0;


	char c = file.get();
	int numVal;
	while(file.good() && !file.eof()){
		++line;
		numVal=0;
		bool previousWasBlank=true;
		while (numVal<6 && c!='\n'){
			if(c==' ' || c=='\t'){
				if(!previousWasBlank) ++numVal;
				previousWasBlank=true;
			} else previousWasBlank=false;
			if(numVal<6) c = file.get();
		}

		if(numVal==6){

			//now comes the data!
			long thisSNP=0;
			snp* thisSNPPointer;
			unsigned long SNPcounter=0;
			bool firstRead=false;
			bool secondRead=false;
			char first, second;
			myIndividuals.push_back(new individual());
			while (c!='\n'){
				c = file.get();
				if(c==' ' || c=='\t' || c=='\n'){
					if(firstRead && secondRead){
						if(SNPcounter==SNPok.size()) throw "Too many values in ped file '" + filename + " on line " + toString(line) + ".";
						if(SNPok[SNPcounter]){
							thisSNPPointer=mySnpDB->getSNP(snpIDs.at(thisSNP));
							if(!myIndividuals[nIndividuals]->addSNP(first, second, thisSNPPointer->allele1, thisSNPPointer->allele2))
								throw "Allele mismatch for SNP '"+ thisSNPPointer->name +"' in individual " + toString(nIndividuals) + ": '" + toString(first) + "/" + toString(second) + "' instead of '" + toString(thisSNPPointer->allele1) + "' or '" + toString(thisSNPPointer->allele2) + "'.";
							++thisSNP;
						}
						++SNPcounter;
						firstRead=false;
						secondRead=false;
					}
					previousWasBlank=true;
				} else {
					if((firstRead && previousWasBlank) || secondRead){
						second=c;
						secondRead=true;
					} else {
						first=c;
						firstRead=true;
					}
					previousWasBlank=false;
				}
			}
			if(SNPcounter!=SNPok.size()) throw "Too few values in ped file '" + filename + " on line " + toString(line) + ".";
			++nIndividuals;
		}
		c = file.get();
	}
	file.close();
	logfile->write("done (" + toString( (clock() - start)/CLOCKS_PER_SEC ) + "s)!");
	logfile->conclude("Read " + toString(nIndividuals) + " individuals and " + toString(nSNPs) + " SNPs.");
	genotypingRate = getGenotypingRate();
	logfile->conclude("Overall genotyping rate is " + toString(genotypingRate) + ".");
	logfile->endIndent();
}
//---------------------------------------------------------------------------
double data::getGenotypingRate(){
	long m=0;
	for(int i=0; i<nIndividuals;++i) m=m+myIndividuals[i]->getNumberMissingAlleles();
	return (double) 1-( (double) m/(2*nIndividuals*nSNPs));
}
//---------------------------------------------------------------------------
std::string data::getFirstChr(){
	return mySnpDB->getSNP(snps.begin()->first)->chromosome;
}
//---------------------------------------------------------------------------
bool data::snpIsTyped(long id){
	if(snps.find(id)==snps.end()) return false;
	else return true;
}
//---------------------------------------------------------------------------
void data::fillChromosomeList(){
	logfile->listFlush("Building chromosome list ...");
	chromosomes.clear();
	std::map<long, long>::iterator snpIt;
	snp* mySnp;
	for(snpIt=snps.begin();snpIt!=snps.end();++snpIt){
		mySnp=mySnpDB->getSNP(snpIt->first);
		chromosomes.insert(mySnp->chromosome);
	}
	logfile->write(" done!");
}
//---------------------------------------------------------------------------
void data::fillSNPmap(std::string chr){
	snpMap.clear();
	snpMapID.clear();
	snp* mySnp;
	std::map<long, long>::iterator snpIt;
	for(snpIt=snps.begin();snpIt!=snps.end();++snpIt){
		mySnp=mySnpDB->getSNP(snpIt->first);
		if(mySnp->chromosome==chr){
			snpMap.insert(std::pair<long, snp*>(mySnp->position, mySnp));
			snpMapID.insert(std::pair<long,long>(mySnp->position, snpIt->second));
		}
	}
}
//---------------------------------------------------------------------------
void data::fillHaplotypeArray(bool* array, long snpId){
	//is assumed that there is no missing data!!! Test first!
	int pos=snps.find(snpId)->second;
	for(int i=0; i<nIndividuals;++i){
		array[2*i]=myIndividuals[i]->allele1.at(pos);
		array[2*i+1]=myIndividuals[i]->allele2.at(pos);
	}
}
//---------------------------------------------------------------------------
void data::fillGenotypeArray(int* array, long snpId){
	int pos=snps.find(snpId)->second;
	for(int i=0; i<nIndividuals;++i){
		array[i]=myIndividuals[i]->getUnphasedGenotype(pos);
	}
}
//---------------------------------------------------------------------------
void data::writeEIGENSTRAT(std::string basename, bool phased){
	//write Eigenstrat file
	std::ofstream snpfile, genofile;
	std::string filename;
	if(phased) logfile->startIndent("Writing phased EIGENSTRAT files:");
	else logfile->startIndent("Writing unphased EIGENSTRAT files:");

	//do it for all chromosomes -> build list of chromosomes
	fillChromosomeList();

	//go through all chromosomes
	std::set<std::string>::iterator chrIt;
	for(chrIt=chromosomes.begin(); chrIt!=chromosomes.end(); ++chrIt){
		logfile->listFlush("Writing chromosome " + *chrIt + " ... ");
		//build a map of all snps from this chromosome
		fillSNPmap(*chrIt);

		//open files to write to
		if(phased) basename=basename + ".phased";
		else basename=basename + ".unphased";
		filename=basename + ".snps." + *chrIt;
		snpfile.open(filename.c_str());
		if(!snpfile)  throw "Unable to create file " + filename + "!";
		filename=basename+".genotypes." + *chrIt;
		genofile.open(filename.c_str());
		if(!genofile)  throw "Unable to create file " + filename + "!";

		//now go through the SNP map and write the files
		snpMapIDIt=snpMapID.begin();
		if(phased){
			if(genotypingRate<1) std::cerr << "WARNING: Missing genotypes while writing phased EIGENSTRAT!" << std::endl;
			for(snpMapIt=snpMap.begin(); snpMapIt!=snpMap.end();++snpMapIt, ++snpMapIDIt){
				snpfile << snpMapIt->second->chromosome << "\t" << snpMapIt->second->name << "\t" << snpMapIt->second->geneticPosition << "\t" << snpMapIt->second->position << "\t" << snpMapIt->second->allele1 << "\t" << snpMapIt->second->allele2 << std::endl;
				for(int i=0; i<nIndividuals;++i){
					genofile << myIndividuals[i]->getEIGENSTRATphased(snpMapIDIt->second);
				}
				genofile << std::endl;
			}
		} else {
			for(snpMapIt=snpMap.begin(); snpMapIt!=snpMap.end();++snpMapIt, ++snpMapIDIt){
				snpfile << snpMapIt->second->chromosome << "\t" << snpMapIt->second->name << "\t" << snpMapIt->second->geneticPosition << "\t" << snpMapIt->second->position << "\t" << snpMapIt->second->allele1 << "\t" << snpMapIt->second->allele2 << std::endl;
				for(int i=0; i<nIndividuals;++i){
					genofile << myIndividuals[i]->getUnphasedGenotype(snpMapIDIt->second);
				}
				genofile << std::endl;
			}
		}
		logfile->write(toString(snpMap.size()) + " SNPs written!");
		genofile.close();
		snpfile.close();
		snps.clear();
	}
	logfile->endIndent();
}
//---------------------------------------------------------------------------
void data::writeHAPMIXreference(std::string basename){
	//write HAPMIX file
	std::ofstream hapmixRefFile;
	logfile->startIndent("Writing reference population HAPMIX file:");

	//do it for all chromosomes -> build list of chromosomes
	fillChromosomeList();

	//go through all chromosomes
	std::set<std::string>::iterator chrIt;
	for(chrIt=chromosomes.begin(); chrIt!=chromosomes.end(); ++chrIt){
		logfile->listFlush("Writing chromosome " + *chrIt + " ... ");
		//build a map of all snps from this chromosome
		fillSNPmap(*chrIt);

		//open files to write to
		basename=basename + ".HAPMIX.refpop." + *chrIt;
		hapmixRefFile.open(basename.c_str());
		if(!hapmixRefFile)  throw "Unable to create file " + basename + "!";

		//write header
		hapmixRefFile << ":sites:" << snpMapID.size() << std::endl;
		hapmixRefFile << ":sequences:" << 2*nIndividuals << std::endl;
		snpMapIt=snpMap.begin();
		hapmixRefFile << snpMapIt->second->position;
		++snpMapIt;
		for(; snpMapIt!=snpMap.end();++snpMapIt){
			hapmixRefFile << " " << snpMapIt->second->position;
		}
		hapmixRefFile << std::endl;

		//now go through all individuals (each haplotype is on one line)
		for(int i=0; i<nIndividuals;++i){
			//write both haplotypes
			for(snpMapIDIt=snpMapID.begin(); snpMapIDIt!=snpMapID.end(); ++snpMapIDIt){
				hapmixRefFile << myIndividuals[i]->getHAPMIXhaplotype1(snpMapIDIt->second);
			}
			hapmixRefFile << std::endl;
			for(snpMapIDIt=snpMapID.begin(); snpMapIDIt!=snpMapID.end(); ++snpMapIDIt){
				hapmixRefFile << myIndividuals[i]->getHAPMIXhaplotype2(snpMapIDIt->second);
			}
			hapmixRefFile << std::endl;
		}
		logfile->write(toString(snpMap.size()) + " SNPs written!");
		hapmixRefFile << std::endl;
		hapmixRefFile.close();
	}
	logfile->endIndent();
}
//---------------------------------------------------------------------------
void data::writeHAPMIXadmixed(std::string basename){
	//write HAPMIX files for the admixed population:
	//  - 1 snp file per chr
	//  - 1 genotype file per individual and chr

	std::ofstream hapmixFile;
	std::string filename;
	logfile->startIndent("Writing reference population HAPMIX file:");

	//do it for all chromosomes -> build list of chromosomes
	fillChromosomeList();

	//go through all chromosomes
	std::set<std::string>::iterator chrIt;
	for(chrIt=chromosomes.begin(); chrIt!=chromosomes.end(); ++chrIt){
		logfile->listFlush("Writing chromosome " + *chrIt + " ... ");
		//build a map of all snps from this chromosome
		fillSNPmap(*chrIt);

		//write snp file
		basename=basename + ".HAPMIX.admpop.";
		filename=basename + "snps." + *chrIt;
		hapmixFile.open(filename.c_str());
		if(!hapmixFile)  throw "Unable to create file " + basename + "!";
		hapmixFile << ":sites:" << snpMapID.size() << std::endl;
		snpMapIt=snpMap.begin();
		hapmixFile << snpMapIt->second->position;
		++snpMapIt;
		for(; snpMapIt!=snpMap.end();++snpMapIt){
			hapmixFile << " " << snpMapIt->second->position;
		}
		hapmixFile << std::endl;
		hapmixFile.close();

		//now go through all individuals (each individual to a seperate file)
		for(int i=0; i<nIndividuals;++i){
			//open file
			filename=basename + "ind" + toString(i) + "." + *chrIt;
			hapmixFile.open(filename.c_str());
			if(!hapmixFile)  throw "Unable to create file " + basename + "!";

			//write header
			hapmixFile << ":sequences:2" << std::endl;
			//write genotypes
			for(snpMapIDIt=snpMapID.begin(); snpMapIDIt!=snpMapID.end(); ++snpMapIDIt){
				hapmixFile << myIndividuals[i]->getHAPMIXgenotype(snpMapIDIt->second);
			}
			hapmixFile << std::endl;
			hapmixFile.close();
		}
		logfile->write(toString(snpMap.size()) + " SNPs written!");
	}
	logfile->endIndent();
}
















