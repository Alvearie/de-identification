/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.State;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.StatesUSMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.StatesUSManager;

public class StatesUSMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -5570733724994623092L;

  protected volatile StatesUSManager statesUSManager = null;

  protected volatile boolean initialized = false;

  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  /**
   * Instantiates a new masking provider for names of states of the United States.
   *
   * @param configuration the configuration
   * @param tenantId tenant associated with the current request
   */
  public StatesUSMaskingProvider(StatesUSMaskingProviderConfig configuration, String tenantId) {
    unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  protected void initialize() {
    if (!initialized) {
      statesUSManager =
          (StatesUSManager) ManagerFactory.getInstance().getManager(null, Resource.STATES_US, null);
      initialized = true;
    }
  }

  @Override
  public String mask(String identifier) {
    initialize();

    if (identifier == null) {
      debugFaultyInput("null");
      return null;
    }

    State state = statesUSManager.getKey(identifier);

    if (state == null) {
      debugFaultyInput("state");
      if (unspecifiedValueHandling == 2) {
        State randomState = statesUSManager.getRandomValue();
        return randomState == null ? null : randomState.getName();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    State randomState = statesUSManager.getRandomValue(state.getNameCountryCode());
    return randomState == null ? null : randomState.toString(state.getNameFormat());
  }
}
