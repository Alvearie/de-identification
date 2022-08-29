/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.MaintainMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class AbstractMaskingProviderTest {

  protected static class TestAbstractMaskingProvider extends AbstractMaskingProvider {

    private static final long serialVersionUID = -3324706543383225218L;

    public TestAbstractMaskingProvider(MaskingProviderConfig config) {
      super("t1", LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES, config);
      setName("theRuleName");
    }

    @Override
    public String mask(String identifier) {
      return null;
    }
  }

  @Test
  public void testApplyUnexpectedValueHandling() {

    // new NULL
    MaskingProviderConfig config = new MaintainMaskingProviderConfig();
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    TestAbstractMaskingProvider provider = new TestAbstractMaskingProvider(config);
    assertNull(provider.applyUnexpectedValueHandling("input", null));

    // new RANDOM, not supported
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    provider = new TestAbstractMaskingProvider(config);
    assertNull(provider.applyUnexpectedValueHandling("input", null));

    // new RANDOM, with support
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    provider = new TestAbstractMaskingProvider(config);
    assertEquals("random-value",
        provider.applyUnexpectedValueHandling("input", () -> "random-value"));

    // new MESSAGE, no message set
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    provider = new TestAbstractMaskingProvider(config);
    assertEquals("OTHER", provider.applyUnexpectedValueHandling("input", () -> "random-value"));

    // new MESSAGE, custom message
    config.setUnexpectedInputReturnMessage("msg1");
    provider = new TestAbstractMaskingProvider(config);
    assertEquals("msg1", provider.applyUnexpectedValueHandling("input", () -> "random-value"));

    // new MESSAGE, empty string
    config.setUnexpectedInputReturnMessage("");
    provider = new TestAbstractMaskingProvider(config);
    assertEquals("", provider.applyUnexpectedValueHandling("input", () -> "random-value"));

    // new MESSAGE, null
    config.setUnexpectedInputReturnMessage(null);
    provider = new TestAbstractMaskingProvider(config);
    assertNull(provider.applyUnexpectedValueHandling("input", () -> "random-value"));

    // new EXIT
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.ERROR_EXIT);
    provider = new TestAbstractMaskingProvider(config);
    try {
      provider.applyUnexpectedValueHandling("Input-value", null);
      fail("expected exception");
    } catch (PrivacyProviderInvalidInputException e) {
      assertFalse(e.getMessage(), e.getMessage().contains("Input-value"));
      assertTrue(e.getMessage(), e.getMessage().contains("theRuleName"));
    }

  }

  @Test
  public void testIsUnexpectedValueHandlingRandom() {

    TestAbstractMaskingProvider provider;
    MaskingProviderConfig config = new MaintainMaskingProviderConfig();
    provider = new TestAbstractMaskingProvider(config);
    assertFalse(provider.isUnexpectedValueHandlingRandom());

    for (UnexpectedMaskingInputHandler handler : UnexpectedMaskingInputHandler.values()) {
      config.setUnexpectedInputHandling(handler);
      provider = new TestAbstractMaskingProvider(config);
      boolean random = provider.isUnexpectedValueHandlingRandom();
      if (handler == UnexpectedMaskingInputHandler.RANDOM) {
        assertTrue(random);
      } else {
        assertFalse(random);
      }
    }
  }
}
