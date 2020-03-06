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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import javax.ws.rs.core.Response.Status;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;

import eu.internetofus.wenet_interaction_protocol_engine.persistence.NormsRepository;
import eu.internetofus.wenet_interaction_protocol_engine.services.WeNetProfileManagerService;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.OperationRequest;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the {@link NormsResource}.
 *
 * @see NormsResource
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(VertxExtension.class)
public class NormsResourceTest {

	/**
	 * Create a resource where the repository is a mocked class.
	 *
	 * @return the created class with the mocked repository.
	 */
	public static NormsResource createNormsResource() {

		final NormsResource resource = new NormsResource();
		resource.repository = mock(NormsRepository.class);
		resource.profileManager = mock(WeNetProfileManagerService.class);
		return resource;

	}

	/**
	 * Check fail create published norm because repository can not store it.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldFailCreatePublishedNormBecasueRepositoryFailsToStore(VertxTestContext testContext) {

		final NormsResource resource = createNormsResource();
		final OperationRequest context = mock(OperationRequest.class);
		resource.publishNorm(PublishedNormTest.createMinimumValidPublishedNormExample().toJsonObject(), context,
				testContext.succeeding(create -> {

					assertThat(create.getStatusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
					testContext.completeNow();
				}));

		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<PublishedNorm>>> storeHandler = ArgumentCaptor.forClass(Handler.class);
		verify(resource.repository, times(1)).storePublishedNorm(any(), storeHandler.capture());
		storeHandler.getValue().handle(Future.failedFuture("Search published norm error"));

	}

	/**
	 * Check fail update publishedNorm because repository can not update it.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldFailUpdatePublishedNormBecasueRepositoryFailsToUpdate(VertxTestContext testContext) {

		final NormsResource resource = createNormsResource();
		final OperationRequest context = mock(OperationRequest.class);
		resource.updatePublishedNorm("publishedNormId", new JsonObject().put("name", "PublishedNorm name"), context,
				testContext.succeeding(update -> {

					assertThat(update.getStatusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
					testContext.completeNow();
				}));

		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<PublishedNorm>>> searchHandler = ArgumentCaptor.forClass(Handler.class);
		verify(resource.repository, times(1)).searchPublishedNorm(any(), searchHandler.capture());
		searchHandler.getValue().handle(Future.succeededFuture(PublishedNormTest.createMinimumValidPublishedNormExample()));
		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<PublishedNorm>>> updateHandler = ArgumentCaptor.forClass(Handler.class);
		verify(resource.repository, times(1)).updatePublishedNorm(any(), updateHandler.capture());
		updateHandler.getValue().handle(Future.failedFuture("Update published norm error"));

	}

}
