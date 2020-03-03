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

package eu.internetofus.wenet_interaction_protocol_engine.api.communities;

import eu.internetofus.wenet_interaction_protocol_engine.ModelTestCase;
import eu.internetofus.wenet_interaction_protocol_engine.services.WeNetProfileManagerService;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;

/**
 * Test the {@link CommunityMember}
 *
 * @see CommunityMember
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class CommunityMemberTest extends ModelTestCase<CommunityMember> {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public CommunityMember createModelExample(int index) {

		final CommunityMember model = new CommunityMember();
		model.userId = null;
		model.joinTime = index * 100000;
		return model;
	}

	/**
	 * Create a community member creating before the user on the WeNet.
	 *
	 * @param index          of the community user to create.
	 * @param profileManager service to create profile users.
	 *
	 * @return the future created community member.
	 */
	public Future<CommunityMember> createModelExample(int index, WeNetProfileManagerService profileManager) {

		final Promise<CommunityMember> promise = Promise.promise();
		profileManager.createProfile(new JsonObject(), create -> {

			if (create.failed()) {

				promise.fail(create.cause());

			} else {

				final CommunityMember model = new CommunityMember();
				model.userId = create.result().getString("id");
				model.joinTime = index * 100000;
				promise.complete(model);
			}

		});
		return promise.future();
	}

}
