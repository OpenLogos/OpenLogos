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
#ifndef _CacheTargetDataClass
#define _CacheTargetDataClass

#define TARGET_KEYLEN 16	// sizeof(usage_id) + 3 * sizeof(int)

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct d_row {
	char word[79];
	int usage_id;	// the key
	int word_id;
	int word_count;
	int word_type_code;
	int head_word;
	int black_hole_location;
	int wildcard_position;
	char aspire_switch[2];
	char pat_exception_switch[2];
	int word_class_code;
	int gender_code;
	int numeric_constraint;
	int aux_code;
	int pat_number;
	int verb_prefix_insep;
	int verb_prefix_sep;
	int verb_prefix_insep_len;
	int verb_prefix_sep_len;
	int source_stem_number;
	int root_usage_id;
	int source_anal_code;
};

class DLLEXPORT CacheTargetData : public CacheData {

public:

	CacheTargetData(SqlConnection *con, int langcode, bool loader, bool ff, bool sav);
	void makeKey(CacheKey *, int usid, int val);
	virtual bool 	populateFromDatabase();
	virtual void getSizeFromDB();
	inline struct d_row * getRow(int i) {
		struct d_row *drp = 
		(struct d_row *)((char *)data_array + i*sizeof(struct d_row));
		return drp;
	}

// Statement D
// select w.Word_ID, w.Word, w.Word_Count, w.Word_Type_Code, w.Head_Word,  
//	w.Black_Hole_Location, w.Wildcard_Position, w.Aspire_Switch,  
//	w.Pat_Exception_Switch, m.Word_Class_Code, m.Pat_Number,
//	m.Gender_Code, m.Numeric_Constraint, m.Auxiliary_Code,
//	m.Verb_Prefix_Inseparable, m.Verb_Prefix_Separable,
//	m.Verb_Prefix_Inseparable_Length, m.Verb_Prefix_Separable_Length,
//	m.Source_Stem_Number, m.Root_Usage_ID, m.Source_Analysis_Code  
// from
//	Word_Phrase w, Morphology m
// where
//	m.Usage_ID = :1
//	and w.Company_Code = m.Company_Code
//	and w.Word_ID = m.word_id

	inline int getUsageId(int i) { return getRow(i)->usage_id; }
	inline int getWordId(int i) { return getRow(i)->word_id; }
	inline char * getWord(int i) { return getRow(i)->word; }
	inline int getWordCount(int i) { return getRow(i)->word_count; }
	inline int getWordTypeCode(int i) { return getRow(i)->word_type_code; }
	inline int getHeadWord(int i) { return getRow(i)->head_word; }
	inline int getBlackHoleLocation(int i) {
		return getRow(i)->black_hole_location; }
	inline int getWildcardPosition(int i) {
		return getRow(i)->wildcard_position; }
	inline char *getAspireSwitch(int i) {
		return getRow(i)->aspire_switch; }
	inline char *getPatExceptionSwitch(int i) {
		return getRow(i)->pat_exception_switch; }
	inline int getWordClassCode(int i) {
		return getRow(i)->word_class_code; }
	inline int getPatNumber(int i) { return getRow(i)->pat_number; }
	inline int getGenderCode(int i) { return getRow(i)->gender_code; }
	inline int getNumericConstraint(int i) {
		return getRow(i)->numeric_constraint; }
	inline int getAuxCode(int i) { return getRow(i)->aux_code; }
	inline int getVerbPrefixInsep(int i) {
		return getRow(i)->verb_prefix_insep; }
	inline int getVerbPrefixSep(int i) {
		return getRow(i)->verb_prefix_sep; }
	inline int getVerbPrefixInsepLen(int i) {
		return getRow(i)->verb_prefix_insep_len; }
	inline int getVerbPrefixSepLen(int i) {
		return getRow(i)->verb_prefix_sep_len; }
	inline int getSourceStemNumber(int i) {
		return getRow(i)->source_stem_number; }
	inline int getRootUsageId(int i) { return getRow(i)->root_usage_id;}
	inline int getSourceAnalCode(int i) {
		return getRow(i)->source_anal_code;}
};

#endif // _CacheTargetDataClass
