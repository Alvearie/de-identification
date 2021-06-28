/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.State;
import com.ibm.whc.deid.models.StateNameFormat;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.StatesUSMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.StatesUSManager;

public class StatesUSMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -5570733724994623092L;

  protected transient volatile StatesUSManager statesResourceManager = null;

  /**
   * Instantiates a new StateUS masking provider.
   * 
   * @param configuration the configuration of this privacy provider
   * @param tenantId the identifier of the tenant associated with this request
   * @param localizationProperty location of the localization property file
   */
  public StatesUSMaskingProvider(StatesUSMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
  }

  protected StatesUSManager getStateManager() {
    if (statesResourceManager == null) {
      statesResourceManager = (StatesUSManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.STATES_US, null, localizationProperty);
    }
    return statesResourceManager;
  }

  @Override
  public String mask(String identifier) {

    StatesUSManager statesUSManager = getStateManager();

    State state = statesUSManager.getValue(identifier);

    if (state == null) {
      State selectedState = statesUSManager.getRandomValue();
      return selectedState == null ? null : selectedState.toString(StateNameFormat.FULL_NAME);
    }

    State randomState = statesUSManager.getRandomValue();
    return randomState == null ? null : randomState.toString(state.getNameFormat());
  }
}
