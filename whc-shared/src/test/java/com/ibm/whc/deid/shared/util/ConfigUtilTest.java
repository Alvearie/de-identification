/*
 * © Merative US L.P. 2023
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.util;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.DeidConfig;

public class ConfigUtilTest {

  @Test
  public void testMain() throws DeidException {
    DeidConfig dc = ConfigUtil.getDeidConfig();
    assertEquals("com.ibm.whc.deid.providers.masking.BasicMaskingProviderFactory",
        dc.getMaskingProviderFactoryClass());
    assertEquals("com.ibm.whc.deid.providers.masking.ComplexMaskingProviderFactory",
        dc.getComplexMaskingProviderFactoryClass());
    assertEquals("com.ibm.whc.deid.providers.identifiers.BuiltInIdentifierFactory",
        dc.getIdentifierFactoryClass());
    assertEquals("com.ibm.whc.deid.ObjectMapperUtil", dc.getObjectMapperUtilClass());
  }
}
