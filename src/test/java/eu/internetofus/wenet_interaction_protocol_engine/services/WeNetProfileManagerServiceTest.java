/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.services;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the {@link WeNetProfileManagerService}.
 *
 * @see WeNetProfileManagerService
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class WeNetProfileManagerServiceTest {

	/**
	 * Should not create a bad profile.
	 *
	 * @param service     to check that it can not create the profile.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldNotCreateBadProfile(WeNetProfileManagerService service, VertxTestContext testContext) {

		service.createProfile(new JsonObject().put("undefinedField", "value"), testContext.failing(handler -> {
			testContext.completeNow();

		}));

	}

	/**
	 * Should not retrieve undefined profile.
	 *
	 * @param service     to check that it can not create the profile.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldNotRretrieveUndefinedProfile(WeNetProfileManagerService service, VertxTestContext testContext) {

		service.retrieveProfile("undefined-profile-identifier", testContext.failing(handler -> {
			testContext.completeNow();

		}));

	}

	/**
	 * Should not delete undefined profile.
	 *
	 * @param service     to check that it can not create the profile.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldNotDeleteUndefinedProfile(WeNetProfileManagerService service, VertxTestContext testContext) {

		service.deleteProfile("undefined-profile-identifier", testContext.failing(handler -> {
			testContext.completeNow();

		}));

	}

	/**
	 * Should retrieve created profile.
	 *
	 * @param service     to check that it can not create the profile.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldRetrieveCreatedProfile(WeNetProfileManagerService service, VertxTestContext testContext) {

		service.createProfile(new JsonObject(), testContext.succeeding(create -> {

			final String id = create.getString("id");
			service.retrieveProfile(id, testContext.succeeding(retrieve -> testContext.verify(() -> {

				assertThat(create).isEqualTo(retrieve);
				service.deleteProfile(id, testContext.succeeding(empty -> {

					service.retrieveProfile(id, testContext.failing(handler -> {
						testContext.completeNow();

					}));

				}));

			})));

		}));

	}

}
