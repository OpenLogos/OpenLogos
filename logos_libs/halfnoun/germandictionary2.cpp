/*
This file is part of OpenLogos/LogOSMaTrans.  Copyright (C) 2005 Globalware AG

OpenLogos/LogOSMaTrans has two licensing options:

The Commercial License, which allows you to provide commercial software
licenses to your customers or distribute Logos MT based applications or to use
LogOSMaTran for commercial purposes. This is for organizations who do not want
to comply with the GNU General Public License (GPL) in releasing the source
code for their applications as open source / free software.

The Open Source License allows you to offer your software under an open source
/ free software license to all who wish to use, modify, and distribute it
freely. The Open Source License allows you to use the software at no charge
under the condition that if you use OpenLogos/LogOSMaTran in an application you
redistribute, the complete source code for your application must be available
and freely redistributable under reasonable conditions. GlobalWare AG bases its
interpretation of the GPL on the Free Software Foundation's Frequently Asked
Questions.

OpenLogos is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the License conditions along with this
program. If not, write to Globalware AG, Hospitalstraﬂe 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
//---------------------------------------------------------------------
// File - GermanDictionary2.cpp
//
// class - GermanDictionary - non-database dependent code
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>

#include <logos_libs/utility/iniparser.h>
#include <logos_libs/halfnoun/germandictionary.h>
#include <logos_libs/halfnoun/germandictionaryentry.h>
#include <logos_libs/halfnoun/germanutil.h>
#include <logos_libs/halfnoun/germanbufferlength.h>
#include <logos_libs/halfnoun/logfile.h>

#ifdef HAVE_MMAP
#include <fcntl.h>
#include <sys/mman.h>
#endif

GermanDictionary::GermanDictionary(
    const LgsString& iniFileName, const LgsString& binaryFileName,
    const LgsString& logFileName, const LgsString& textFileName)
    : iniFileName_(iniFileName)
    , binaryFileName_(binaryFileName)
    , textFileName_(textFileName)

{
    wordList_.reserve(INITIAL_SIZE);
    LogFile::initialize(logFileName);
}

int GermanDictionary::longestPrefix(const char* word, int minLength, int maxLength, Range& range) const
{
    char* buffer = GermanUtil::duplicate(word, maxLength);
    int length = longestPrefix_(buffer, minLength, wordList_.end(), range);
    delete[] buffer;
    return length;
}

int GermanDictionary::longestPrefix_(char* word, int minLength, Iterator end, Range& range) const
{
    int wordLength = strlen(word);
    if (wordLength < minLength)
        return 0;

    // find insertion point in sorted vector
    Iterator begin = wordList_.begin();

    // invoke special constructor for entry - does not allocate memory, word is already normalized
    GermanDictionaryEntry entry(word, GermanDictionaryEntry::Dummy());
    range = equal_range(begin, end, entry);

    if (range.first != range.second)             // word has maximum prefix
        return wordLength;

    if (range.first == begin)                    // insertion point at start - no match
        return 0;

    // look at previous entry and determine common prefix length with word
    Iterator iter = range.first - 1;
    int commonPrefixLength = GermanUtil::calcCommonPrefixLength(iter->getNormalizedText(), word);
    assert(commonPrefixLength < wordLength);

    if (commonPrefixLength < minLength)          // common prefix too short
        return 0;

    // search recursively for smaller prefix in smaller sub-range of the wordList_
    word[commonPrefixLength] = 0;
    return longestPrefix_(word, minLength, iter + 1, range);
}

void GermanDictionary::sortCache()
{
    Timer timer(LgsString("sortCache"));

    sort(wordList_.begin(), wordList_.end());
}

void GermanDictionary::streamIn()
{
  assert(wordList_.size() == 0);

  Timer timer(LgsString("streamIn"));

  //ocr
  DWORD desired_access;
  char *cp = NULL, *name = (char *)binaryFileName_.c_str();
#ifndef HAVE_MMAP
  HANDLE hdl;

  int n = strlen(name);
  // I have to replace backslashes w/ smthg else, because
  // memory mapping functions dont want them.
  char *fname = new char[n+1];
  strcpy(fname, name);
  for(int i=0;i<n;i++)
    if(fname[i]=='\\')
      fname[i] = '/';

  desired_access = FILE_MAP_READ;
  hdl = OpenFileMapping(desired_access, false, fname);
	
  cp = NULL;
  if(hdl) {
    cp = (char *)MapViewOfFile(hdl, desired_access, 0, 0, 0);
    if(cp==NULL) {
      fprintf(stderr, "streamIn.MapViewOfFile \"%s\" error %d\n", 
              fname, GetLastError());
      CloseHandle(hdl);
    }
  } else {
    fprintf(stderr, "streamIn.OpenFileMapping \"%s\" error %d\n", 
            fname, GetLastError());
  }
  delete [] fname;
#else
  // mmap does not seem to be faster than file I/O
  /*
  int fd = open(name, O_RDONLY);
  off_t len = 0;
  if (fd != 0) {
    off_t len = lseek(fd, 0, SEEK_END);
    if (errno == 0) {
      cp = (char *)mmap(NULL, len, PROT_READ, MAP_SHARED, fd, 0);
    }
  }
  */
#endif
  // ------------------

  istream *input;
  if(cp) {
    int sz = *(int *)cp;
    printf("sz = %d\n", sz);
    input = new stringstream(string(cp + sizeof(int), sz),
                             ios::in | ios::binary);
  } else
    input = new ifstream(name, ios::in | ios::binary);

  if (!input->good())
    throw LgsString("cannot open file ") + binaryFileName_ + LgsString(" for input");

  int totalWords = 0;
  for (;;)
    {
      GermanDictionaryEntry entry;
      if (!entry.readFrom(*input))
        break;
      totalWords++;
      wordList_.push_back(entry);
    }

  cout << "Total words in German Cache = " << totalWords << endl;

  delete input;
#ifdef HAVE_MMAP
  //if (cp) munmap(cp, len);
  //if (fd) close(fd);
#endif
}

void GermanDictionary::streamOut()
{
    Timer timer(LgsString("streamOut"));

    ofstream output(binaryFileName_.c_str(), ios::out | ios::binary);
    if (!output.good())
        throw LgsString("cannot open file ") + binaryFileName_ + LgsString(" for output");

    for (Iterator iter = wordList_.begin(); iter != wordList_.end(); iter++)
        iter->printOn(output);
}

void GermanDictionary::streamOutAsText()
{
    Timer timer(LgsString("streamOutAsText"));

    if (textFileName_.length() == 0)
        throw LgsString("text file name not provided");

    ofstream output(textFileName_.c_str());
    if (!output.good())
        throw LgsString("cannot open file ") + textFileName_ + LgsString(" for output");

    for (Iterator iter = wordList_.begin(); iter != wordList_.end(); iter++)
        iter->printOnAsText(output);
}

