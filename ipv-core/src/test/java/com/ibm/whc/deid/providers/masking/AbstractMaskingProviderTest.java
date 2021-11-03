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
    
    // no config
    TestAbstractMaskingProvider provider = new TestAbstractMaskingProvider(null);
    assertNull(provider.applyUnexpectedValueHandling("input", null));
    
    // new NULL
    MaskingProviderConfig config = new MaintainMaskingProviderConfig();
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    provider = new TestAbstractMaskingProvider(config);
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

    // old default
    config.setUnexpectedInputHandling(null);
    provider = new TestAbstractMaskingProvider(config);
    assertNull(provider.applyUnexpectedValueHandling("input", null));

    // old NULL
    config.setUnspecifiedValueHandling(1);
    provider = new TestAbstractMaskingProvider(config);
    assertNull(provider.applyUnexpectedValueHandling("input", null));

    // old out of range negative
    config.setUnspecifiedValueHandling(-1);
    provider = new TestAbstractMaskingProvider(config);
    assertNull(provider.applyUnexpectedValueHandling("input", null));

    // old out of range 4
    config.setUnspecifiedValueHandling(4);
    provider = new TestAbstractMaskingProvider(config);
    assertNull(provider.applyUnexpectedValueHandling("input", null));

    // old RANDOM, not supported
    config.setUnspecifiedValueHandling(2);
    provider = new TestAbstractMaskingProvider(config);
    assertNull(provider.applyUnexpectedValueHandling("input", null));

    // old RANDOM, supported
    config.setUnspecifiedValueHandling(2);
    provider = new TestAbstractMaskingProvider(config);
    assertEquals("old random", provider.applyUnexpectedValueHandling("input", () -> "old random"));

    // old MESSAGE, no message set
    config.setUnspecifiedValueHandling(3);
    provider = new TestAbstractMaskingProvider(config);
    assertEquals("OTHER", provider.applyUnexpectedValueHandling("input", () -> "random-value"));

    // old MESSAGE, custom message
    config.setUnspecifiedValueReturnMessage("msg2");
    provider = new TestAbstractMaskingProvider(config);
    assertEquals("msg2", provider.applyUnexpectedValueHandling("input", () -> "random-value"));

    // old MESSAGE, empty string
    config.setUnspecifiedValueReturnMessage("");
    provider = new TestAbstractMaskingProvider(config);
    assertEquals("", provider.applyUnexpectedValueHandling("input", () -> "random-value"));

    // old MESSAGE, null
    config.setUnspecifiedValueReturnMessage(null);
    provider = new TestAbstractMaskingProvider(config);
    assertNull(provider.applyUnexpectedValueHandling("input", () -> "random-value"));
  }

  @Test
  public void testIsUnexpectedValueHandlingRandom() {

    // no config
    TestAbstractMaskingProvider provider = new TestAbstractMaskingProvider(null);
    assertFalse(provider.isUnexpectedValueHandlingRandom());

    // with config
    MaskingProviderConfig config = new MaintainMaskingProviderConfig();

    for (int i = -1; i < 5; i++) {
      config.setUnspecifiedValueHandling(i);
      provider = new TestAbstractMaskingProvider(config);
      boolean random = provider.isUnexpectedValueHandlingRandom();
      if (i == 2) {
        assertTrue(random);
      } else {
        assertFalse(random);
      }
    }
    // even though old says random, new enum takes precedence
    config.setUnspecifiedValueHandling(2);

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

    // verify random still returns true even when old value is not 2
    config.setUnspecifiedValueHandling(2);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    provider = new TestAbstractMaskingProvider(config);
    assertTrue(provider.isUnexpectedValueHandlingRandom());
  }
}
