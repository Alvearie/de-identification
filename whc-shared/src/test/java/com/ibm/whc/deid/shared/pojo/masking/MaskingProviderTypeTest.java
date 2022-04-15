/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.HashSet;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType.MaskingProviderCategory;

public class MaskingProviderTypeTest {

  @Test
  public void testMain() {
    HashSet<String> identifiers = new HashSet<>(MaskingProviderType.values().length * 2);
    for (MaskingProviderType type : MaskingProviderType.values()) {
      String id = type.getIdentifier();
      assertNotNull(id);
      assertFalse(id.trim().isEmpty());
      assertTrue(identifiers.add(id));  // ensure no identifier duplicated
      MaskingProviderCategory cat = type.getCategory();
      assertNotNull(cat);
      switch (type) {
        case BINNING:
        case CONDITIONAL:
        case GENERALIZE:
        case GUID:
        case HASH:
        case MAINTAIN:
        case NULL:
        case NUMBERVARIANCE:
        case PSEUDONYM:
        case RANDOM:
        case REDACT:
        case REPLACE:
          assertEquals(MaskingProviderCategory.CategoryII, cat);
          break;
        default:
          assertEquals(MaskingProviderCategory.CategoryI, cat);
      }
    }
  }
      
}
