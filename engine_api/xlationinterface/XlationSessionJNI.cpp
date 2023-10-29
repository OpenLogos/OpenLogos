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
#include <engine_api/xlationinterface/xlationsessionmanager.h>
#include <engine_api/xlationinterface/xlationsession.h>
#include <logos_java/GlobalWordsEngineAPI/GlobalWordsEngineAPI.h>

XlationSession *opSession = 0;
unsigned long lastErrorCode = 0;
const wchar_t *lastErrorString = L"";

const wchar_t *LastErrorString(int errorCode);
OperationType GetOperationType(int operationCode);

// -------------------------------------------------------------------------
const wchar_t *LastErrorString(int errorCode)
{
   switch(errorCode)
   {
      case SUCCESS:                 return L"SUCCESS";
      case SESSION_COUNT_EXCEEDED:  return L"SESSION COUNT EXCEEDED";
      case INVALID_SESSION:         return L"INVALID SESSION";
      case SESSION_ALREADY_STARTED: return L"SESSION ALREADY STARTED";
      case INVALID_OPERATION:       return L"INVALID OPERATION";
      case PROXY_ERR_INIT:          return L"PROXY ERR INIT";
      case PROXY_ERR_CREATE:        return L"PROXY ERR CREATE";
      case PROXY_ERR_LANGUAGE:      return L"PROXY ERR LANGUAGE";
      case PROXY_ERR_CALL:          return L"PROXY ERR CALL";
      case PROXY_ERR_SERVER:        return L"PROXY ERR SERVER";
      case PROXY_ERR_FILE:          return L"PROXY ERR FILE";
      default:                      return L"UNKNOWN ERROR";
   }
}

// -------------------------------------------------------------------------
OperationType GetOperationType(int operationCode)
{
   switch(operationCode)
   {
      case 0:  return TRANSLATE_DOC;
      case 1:  return TRANSLATE_TEXT;
      case 2:  return TERM_SEARCH_DOC;
      case 3:  return TERM_SEARCH_TEXT;
      case 4:  return QRY_CONFIGURATION;
      default: return TRANSLATE_DOC;
   }
}

// -------------------------------------------------------------------------
JNIEXPORT jboolean JNICALL Java_logos_1java_GlobalWordsEngineAPI_GlobalWordsEngineAPI_InitializeTranslationSession
  (JNIEnv *javaEnv, jobject javaObj)
{
   try
   {
      opSession = XlationSessionManager::singleton().createXlationSession();
   }
   catch(...)
   {
      opSession = NULL;
   }

   return (opSession == NULL)? JNI_FALSE: JNI_TRUE;
}

// -------------------------------------------------------------------------
JNIEXPORT jboolean JNICALL Java_logos_1java_GlobalWordsEngineAPI_GlobalWordsEngineAPI_StartTranslationSession
  (JNIEnv *javaEnv, jobject javaObj)
{
   jboolean result = JNI_TRUE;
   lastErrorCode = SUCCESS;
   lastErrorString = L"";
   try
   {
      if(!XlationSessionManager::singleton().startXlationSession(opSession, 0))
      {
         result = JNI_FALSE;
      }
      lastErrorCode = opSession->getErrorCode();
      lastErrorString = LastErrorString((int)lastErrorCode);
   }
   catch(...)
   {
      result = JNI_FALSE;
   }
   return result;
}

// -------------------------------------------------------------------------
JNIEXPORT jboolean JNICALL Java_logos_1java_GlobalWordsEngineAPI_GlobalWordsEngineAPI_CloseTranslationSession
  (JNIEnv *javaEnv, jobject javaObj)
{
   jboolean result = JNI_TRUE;
   try
   {
      XlationSessionManager::singleton().freeXlationSession(opSession);
      XlationSessionManager::destroySessionManager();
   }
   catch(...)
   {
      result = JNI_FALSE;
   }
   opSession = NULL;

   return result;
}

// -------------------------------------------------------------------------
JNIEXPORT void JNICALL Java_logos_1java_GlobalWordsEngineAPI_GlobalWordsEngineAPI_SetInputParameter
  (JNIEnv *javaEnv, jobject javaObj, jstring name, jstring value)
{
   const jchar *namePtr = javaEnv->GetStringChars(name, 0);
   const jchar *valuePtr = javaEnv->GetStringChars(value, 0);
   try
   {
      opSession->setInputParameter((const wchar_t *) namePtr,
                                   (const wchar_t *) valuePtr);
   }
   catch(...)
   {
   }
   javaEnv->ReleaseStringChars(name, namePtr);
   javaEnv->ReleaseStringChars(value, valuePtr);
}

// -------------------------------------------------------------------------
JNIEXPORT void JNICALL Java_logos_1java_GlobalWordsEngineAPI_GlobalWordsEngineAPI_SetOperationType
  (JNIEnv *javaEnv, jobject javaObj, jint operationType)
{
   try
   {
      opSession->setOperationType(GetOperationType(operationType));
   }
   catch(...)
   {
   }
}

// -------------------------------------------------------------------------
JNIEXPORT jstring JNICALL Java_logos_1java_GlobalWordsEngineAPI_GlobalWordsEngineAPI_GetOutputValue
  (JNIEnv *javaEnv, jobject javaObj, jstring name)
{
   const jchar *namePtr = javaEnv->GetStringChars(name, 0);
   const jchar *valuePtr = (const jchar *) L"";
   try
   {
      valuePtr = (const jchar *) opSession->getOutputValue((const wchar_t *) namePtr);
   }
   catch(...)
   {
   }
   javaEnv->ReleaseStringChars(name, namePtr);
   return javaEnv->NewString(valuePtr,
                             wcslen((const wchar_t *)valuePtr));
}

// -------------------------------------------------------------------------
JNIEXPORT jint JNICALL Java_logos_1java_GlobalWordsEngineAPI_GlobalWordsEngineAPI_GetLastError
  (JNIEnv *javaEnv, jobject javaObj)
{
   return lastErrorCode;
}

// -------------------------------------------------------------------------
JNIEXPORT jstring JNICALL Java_logos_1java_GlobalWordsEngineAPI_GlobalWordsEngineAPI_GetLastErrorString
  (JNIEnv *javaEnv, jobject javaObj)
{
   return javaEnv->NewString((const jchar *)lastErrorString, 
                             wcslen(lastErrorString));
}
