/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;

import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import com.ibm.whc.deid.models.ICD;
import com.ibm.whc.deid.models.ICDFormat;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/** The type Ic dv 9 manager. */
public class ICDv9Manager implements Manager, Serializable {
  /** */
  private static final long serialVersionUID = -5865782878844917863L;

	protected final Collection<ResourceEntry> resourceICDList;

  protected final MapWithRandomPick<String, ICD> icdByCodeMap;
  protected final MapWithRandomPick<String, ICD> icdByNameMap;

  private static LogManager logger = LogManager.getInstance();
  protected final Resources resourceType = Resource.ICDV9;

  protected final String tenantId;

  	/**
	 * Instantiates a new Ic dv 9 manager.
	 *
	 * @param tenantId
	 * @paramlocalizationProperty location of the localization property file
	 */
	public ICDv9Manager(String tenantId, String localizationProperty) {
    this.tenantId = tenantId;
		resourceICDList = LocalizationManager.getInstance(localizationProperty)
				.getResources(Resource.ICDV9);
    this.icdByCodeMap = new MapWithRandomPick<>(new HashMap<String, ICD>());
    this.icdByNameMap = new MapWithRandomPick<>(new HashMap<String, ICD>());

    readResources(resourceType, tenantId);

    this.icdByCodeMap.setKeyList();
    this.icdByNameMap.setKeyList();
  }

  protected void readResources(Resources resourceType, String tenantId) {
    readICDList(resourceICDList);
  }

  protected void readICDList(Collection<ResourceEntry> entries) {

    for (ResourceEntry entry : entries) {
      InputStream inputStream = entry.createStream();

      try (CSVParser parser = Readers.createCSVReaderFromStream(inputStream, ';', '"')) {
        for (CSVRecord record : parser) {
          String code = record.get(0);
          String shortName = record.get(1);
          String fullName = record.get(2);
          String chapterCode = record.get(3);
          String chapterName = record.get(4);
          String categoryCode = record.get(5);
          String categoryName = record.get(6);

          ICD ICDv9CodeRepr = new ICD(code, shortName, fullName, chapterCode, chapterName,
              categoryCode, categoryName, ICDFormat.CODE);
          ICD ICDv9NameRepr = new ICD(code, shortName, fullName, chapterCode, chapterName,
              categoryCode, categoryName, ICDFormat.NAME);

          this.icdByCodeMap.getMap().put(code, ICDv9CodeRepr);
          this.icdByNameMap.getMap().put(shortName.toUpperCase(), ICDv9NameRepr);
          this.icdByNameMap.getMap().put(fullName.toUpperCase(), ICDv9NameRepr);
        }
        inputStream.close();
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }
  }

  @Override
  public String getRandomKey() {
    return this.icdByCodeMap.getRandomKey();
  }

  /**
   * Lookup icd icd.
   *
   * @param codeOrName the code or name
   * @return the icd
   */
  public ICD lookupICD(String codeOrName) {
    String key = codeOrName.toUpperCase();
    ICD ICDv9;

    if ((ICDv9 = this.icdByCodeMap.getMap().get(key)) != null) {
      return ICDv9;
    } else if ((ICDv9 = this.icdByNameMap.getMap().get(key)) != null) {
      return ICDv9;
    }

    return null;
  }

  @Override
  public boolean isValidKey(String codeOrName) {
    return lookupICD(codeOrName) != null;
  }

}
