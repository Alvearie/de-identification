/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util.localization;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class ResourceEntryTest {
  private static final String NON_EXISTING_FILE_NAME = "NON_EXISTING_FILE_NAME";
  private static final String COUNTRY_CODE = "COUNTRY_CODE";

  @Test(expected = RuntimeException.class)
  public void getMethods() throws Exception {

    ResourceEntry entry = new ResourceEntry(NON_EXISTING_FILE_NAME, COUNTRY_CODE,
        ResourceEntryType.INTERNAL_RESOURCE);
    assertNull(entry.createStream());
    assertEquals("getCountryCode", COUNTRY_CODE, entry.getCountryCode());

    entry = new ResourceEntry(NON_EXISTING_FILE_NAME, COUNTRY_CODE,
        ResourceEntryType.EXTERNAL_FILENAME);
    assertNull(entry.createStream());
  }
}
