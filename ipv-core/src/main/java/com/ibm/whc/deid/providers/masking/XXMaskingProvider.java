// @formatter:off
/*******************************************************************************
 * IBM Confidential OCO Source Materials
 * 5737-D31, 5737-A56
 * (C) Copyright IBM Corp. 2021
 *
 * The source code for this program is not published or otherwise
 * divested of its trade secrets, irrespective of what has
 * been deposited with the U.S. Copyright Office.
 *******************************************************************************/
// @formatter:on
package com.ibm.whc.deid.providers.masking;

import com.privacylogistics.FF3Cipher;

/**
 * Temporary hook for testing.
 */
public class XXMaskingProvider {
  // @formatter:off
  @SuppressWarnings("unused")
  private static final String copyright =
      "IBM Confidential OCO Source Materials 5737-D31, 5737-A56 "
          + "(C) Copyright IBM Corp. 2021 "
          + "The source code for this program is not published or otherwise "
          + "divested of its trade secrets, irrespective of what has "
          + "been deposited with the U.S. Copyright Office".intern();
  // @formatter:on  

  private FF3Cipher cipher = new FF3Cipher("11111111222222223333333344444444", "1111222233334444");
}
