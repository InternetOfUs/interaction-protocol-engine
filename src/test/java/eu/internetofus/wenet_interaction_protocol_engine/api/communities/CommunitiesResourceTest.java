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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import javax.ws.rs.core.Response.Status;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;

import eu.internetofus.wenet_interaction_protocol_engine.Model;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.CommunitiesRepository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.OperationRequest;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the {@link CommunitiesResource}.
 *
 * @see CommunitiesResource
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class CommunitiesResourceTest {

	/**
	 * Create a resource where the repository is a mocked class.
	 *
	 * @return the created class with the mocked repository.
	 */
	public static CommunitiesResource createCommunitiesResource() {

		final CommunitiesResource resource = new CommunitiesResource();
		resource.repository = mock(CommunitiesRepository.class);
		return resource;

	}

	/**
	 * Check fail create community because repository can not store it.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldFailCreateCommunityBecasueRepositoryFailsToStore(VertxTestContext testContext) {

		final CommunitiesResource resource = createCommunitiesResource();
		final OperationRequest context = mock(OperationRequest.class);
		resource.createCommunity(new JsonObject(), context, testContext.succeeding(create -> {

			assertThat(create.getStatusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
			testContext.completeNow();
		}));

		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<Community>>> storeHandler = ArgumentCaptor.forClass(Handler.class);
		verify(resource.repository, times(1)).storeCommunity(any(), storeHandler.capture());
		storeHandler.getValue().handle(Future.failedFuture("Search community error"));

	}

	/**
	 * Check fail update community because repository can not update it.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldFailUpdateCommunityBecasueRepositoryFailsToUpdate(VertxTestContext testContext) {

		final CommunitiesResource resource = createCommunitiesResource();
		final OperationRequest context = mock(OperationRequest.class);
		resource.updateCommunity("communityId", new JsonObject().put("name", "Community name"), context,
				testContext.succeeding(update -> {

					assertThat(update.getStatusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
					testContext.completeNow();
				}));

		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<Community>>> searchHandler = ArgumentCaptor.forClass(Handler.class);
		verify(resource.repository, times(1)).searchCommunity(any(), searchHandler.capture());
		searchHandler.getValue().handle(
				Future.succeededFuture(Model.fromJsonObject(new JsonObject().put("_id", "communityId"), Community.class)));
		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<Community>>> updateHandler = ArgumentCaptor.forClass(Handler.class);
		verify(resource.repository, times(1)).updateCommunity(any(), updateHandler.capture());
		updateHandler.getValue().handle(Future.failedFuture("Update community error"));

	}

}
