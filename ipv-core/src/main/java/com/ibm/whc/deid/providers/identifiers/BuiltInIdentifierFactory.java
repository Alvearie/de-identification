/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import com.ibm.whc.deid.identifiers.EntityType;
import com.ibm.whc.deid.identifiers.EntityTypes;

public class BuiltInIdentifierFactory {

	/**
	 * Get an Identifier based on the input type
	 *
	 * @param type
	 * @param localizationProperty TODO
	 * @return
	 */
	public Identifier getIdentifier(EntityTypes type, String tenantId, String localizationProperty) {
		switch ((EntityType) type) {
		case ADDRESS:
			return new AddressIdentifier();
		case ATC:
			return new ATCIdentifier(tenantId, localizationProperty);
		case CITY:
			return new CityIdentifier(tenantId, localizationProperty);
		case CONTINENT:
			return new ContinentIdentifier(tenantId, localizationProperty);
		case COUNTRY:
			return new CountryIdentifier(tenantId, localizationProperty);
		case COUNTY:
			return new CountyIdentifier(tenantId, localizationProperty);
		case CREDIT_CARD:
			return new CreditCardIdentifier();
		case CREDIT_CARD_TYPE:
			return new CreditCardTypeIdentifier();
		case DATETIME:
			return new DateTimeIdentifier();
		case EMAIL:
			return new EmailIdentifier();
		case GENDER:
			return new GenderIdentifier();
		case HOSPITAL:
			return new HospitalIdentifier(tenantId, localizationProperty);
		case IBAN:
			return new IBANIdentifier();
		case ICDV9:
			return new ICDv9Identifier();
		case ICDV10:
			return new ICDv10Identifier();
		case IMEI:
			return new IMEIIdentifier();
		case IP_ADDRESS:
			return new IPAddressIdentifier();
		case LATITUDE_LONGITUDE:
			return new LatitudeLongitudeIdentifier();
		case MAC_ADDRESS:
			return new MACAddressIdentifier();
		case MARITAL_STATUS:
			return new MaritalStatusIdentifier();
		case NAME:
			return new NameIdentifier();
		case OCCUPATION:
			return new OccupationIdentifier();
		case RACE:
			return new RaceEthnicityIdentifier();
		case RELIGION:
			return new ReligionIdentifier();
		case STATES_US:
			return new StatesUSIdentifier();
		case SWIFT:
			return new SWIFTCodeIdentifier();
		case VIN:
			return new VINIdentifier();
		case ZIPCODE:
			return new ZIPCodeIdentifier();
		default:
			throw new IllegalArgumentException("Unknown type " + type);
		}
	}

	public Collection<Identifier> getAvailableIdentifiers(String tenantId, String localizationProperty) {
		Set<Identifier> identifiers = new HashSet<Identifier>();

		Arrays.stream(EntityType.values()).forEach(e -> {
			identifiers.add(getIdentifier(e, tenantId, localizationProperty));
		});

		return identifiers;

	}

}
