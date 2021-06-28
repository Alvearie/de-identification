/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import org.junit.Test;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class ManagerFactoryTest {

  @Test
  public void testCaching() {
    ArrayList<Manager> list = new ArrayList<>();
    String TEST_LOCALIZATION_PROPERTIES = "/localization/test.localization.properties";
    ManagerFactory factory = ManagerFactory.getInstance();

    Manager mgrSwiftDflt = factory.getManager("t2", Resource.SWIFT, null,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);
    assertNotNull(mgrSwiftDflt);
    list.add(mgrSwiftDflt);

    Manager mgrSwiftTest =
        factory.getManager("t2", Resource.SWIFT, null, TEST_LOCALIZATION_PROPERTIES);
    assertNotNull(mgrSwiftTest);
    for (Manager mgr : list) {
      assertFalse(mgr == mgrSwiftTest);
    }
    list.add(mgrSwiftTest);

    Manager mgrATCDflt = factory.getManager("t2", Resource.ATC_CODES, null,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);
    assertNotNull(mgrATCDflt);
    for (Manager mgr : list) {
      assertFalse(mgr == mgrATCDflt);
    }
    list.add(mgrATCDflt);

    // same parameters result in same manager instances returned
    assertTrue(mgrSwiftDflt == factory.getManager("t2", Resource.SWIFT, null,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES));
    
    assertTrue(mgrATCDflt == factory.getManager("t2", Resource.ATC_CODES, null,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES));
    
    assertTrue(mgrSwiftTest == factory.getManager("t2", Resource.SWIFT, null,
        TEST_LOCALIZATION_PROPERTIES));

    // all tenants share same builtin resources
    assertTrue(mgrSwiftDflt == factory.getManager("t1", Resource.SWIFT, null,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES));
    assertTrue(mgrSwiftDflt == factory.getManager("", Resource.SWIFT, null,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES));
    assertTrue(mgrSwiftDflt == factory.getManager(null, Resource.SWIFT, null,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES));
  }
}
