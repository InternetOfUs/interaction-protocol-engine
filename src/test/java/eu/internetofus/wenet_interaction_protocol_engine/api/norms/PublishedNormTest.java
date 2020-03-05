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

package eu.internetofus.wenet_interaction_protocol_engine.api.norms;

import java.util.ArrayList;

import eu.internetofus.wenet_interaction_protocol_engine.ModelTestCase;
import eu.internetofus.wenet_interaction_protocol_engine.TimeManager;
import eu.internetofus.wenet_interaction_protocol_engine.services.WeNetProfileManagerService;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

/**
 * * Test the {@link PublishedNorm}
 *
 * @see PublishedNorm
 *
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class PublishedNormTest extends ModelTestCase<PublishedNorm> {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public PublishedNorm createModelExample(int index) {

		final PublishedNorm model = new PublishedNorm();
		model.name = "Published norm " + index;
		model.description = "Description of published norm " + index;
		model.keywords = new ArrayList<>();
		for (int i = index - 2; i < index + 2; i++) {

			model.keywords.add("keyword " + i);
		}
		model.publisherId = "Published identifier " + index;
		model.publishTime = index * 100000;
		model.norm = new NormTest().createModelExample(index);

		return model;
	}

	/**
	 * Create a valid published norm.
	 *
	 * @param index          of the example to create.
	 * @param profileManager service to manage the profiles.
	 * @param createHandler  component that manage the creation result.
	 *
	 */
	public static void createValidPublishedNormExample(int index, WeNetProfileManagerService profileManager,
			Handler<AsyncResult<PublishedNorm>> createHandler) {

		profileManager.createProfile(new JsonObject(), create -> {

			if (create.failed()) {

				createHandler.handle(Future.failedFuture(create.cause()));

			} else {

				final PublishedNorm result = new PublishedNormTest().createModelExample(index);
				result.publisherId = create.result().getString("id");
				result.publishTime = TimeManager.now();
				createHandler.handle(Future.succeededFuture(result));
			}

		});

	}

}
