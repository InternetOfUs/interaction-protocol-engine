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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import javax.ws.rs.core.Response.Status;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;

import eu.internetofus.wenet_interaction_protocol_engine.Model;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.CommunitiesRepository;
import eu.internetofus.wenet_interaction_protocol_engine.services.WeNetProfileManagerService;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.OperationRequest;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the {@link CommunitiesResource}.
 *
 * @see CommunitiesResource
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(VertxExtension.class)
public class CommunitiesResourceTest {

	/**
	 * Create a resource where the repository is a mocked class.
	 *
	 * @return the created class with the mocked repository.
	 */
	public static CommunitiesResource createCommunitiesResource() {

		final CommunitiesResource resource = new CommunitiesResource();
		resource.repository = mock(CommunitiesRepository.class);
		resource.profileManager = mock(WeNetProfileManagerService.class);
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
		storeHandler.getValue().handle(Future.failedFuture("Store community error"));

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

	/**
	 * Check fail retrieve community member because repository failed.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldFailRetrieveCommunityMembersPageBecauseRepositoryFails(VertxTestContext testContext) {

		final CommunitiesResource resource = createCommunitiesResource();
		final OperationRequest context = mock(OperationRequest.class);
		doReturn(new JsonObject()).when(context).getParams();
		resource.retrieveCommunityMembersPage("communityId", context, testContext.succeeding(update -> {

			assertThat(update.getStatusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
			testContext.completeNow();
		}));

		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<JsonObject>>> searchHandler = ArgumentCaptor.forClass(Handler.class);
		verify(resource.repository, times(1)).searchCommunityMembersPageObject(eq("communityId"), any(), any(), eq(0),
				eq(10), searchHandler.capture());
		searchHandler.getValue().handle(Future.failedFuture("Search community member error"));

	}

	/**
	 * Check fail create community member because repository failed.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldFailCreateCommunityMemberBecauseRepositoryFails(VertxTestContext testContext) {

		final CommunitiesResource resource = createCommunitiesResource();
		final OperationRequest context = mock(OperationRequest.class);
		doReturn(new JsonObject()).when(context).getParams();
		resource.createCommunityMember("communityId", new JsonObject().put("userId", "UserIdentifier"), context,
				testContext.succeeding(update -> {

					assertThat(update.getStatusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
					testContext.completeNow();
				}));

		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<Community>>> searchCommunityHandler = ArgumentCaptor
				.forClass(Handler.class);
		verify(resource.repository, times(1)).searchCommunity(any(), searchCommunityHandler.capture());
		searchCommunityHandler.getValue().handle(Future.succeededFuture(new Community()));
		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<JsonObject>>> retrieveProfileHandler = ArgumentCaptor
				.forClass(Handler.class);
		verify(resource.profileManager, times(1)).retrieveProfile(any(), retrieveProfileHandler.capture());
		retrieveProfileHandler.getValue().handle(Future.succeededFuture(new JsonObject()));
		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<CommunityMember>>> searchCommunityMemberHandler = ArgumentCaptor
				.forClass(Handler.class);
		verify(resource.repository, times(1)).searchCommunityMember(any(), any(), searchCommunityMemberHandler.capture());
		searchCommunityMemberHandler.getValue().handle(Future.failedFuture("The member is not defined"));
		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<JsonObject>>> storeHandler = ArgumentCaptor.forClass(Handler.class);
		verify(resource.repository, times(1)).storeCommunityMemberObject(any(), any(), storeHandler.capture());
		storeHandler.getValue().handle(Future.failedFuture("Can not store member"));

	}

	/**
	 * Check fail create community norm because repository can not store it.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldFailCreateCommunityNormBecasueRepositoryFailsToStore(VertxTestContext testContext) {

		final CommunitiesResource resource = createCommunitiesResource();
		final OperationRequest context = mock(OperationRequest.class);
		resource.createCommunityNorm("communityId", new CommunityNormTest().createModelExample(1).toJsonObject(), context,
				testContext.succeeding(create -> {

					assertThat(create.getStatusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
					testContext.completeNow();
				}));

		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<Community>>> searchHandler = ArgumentCaptor.forClass(Handler.class);
		verify(resource.repository, times(1)).searchCommunity(any(), searchHandler.capture());
		searchHandler.getValue().handle(
				Future.succeededFuture(Model.fromJsonObject(new JsonObject().put("_id", "communityId"), Community.class)));
		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<CommunityNorm>>> storeHandler = ArgumentCaptor.forClass(Handler.class);
		verify(resource.repository, times(1)).storeCommunityNorm(any(), any(), storeHandler.capture());
		storeHandler.getValue().handle(Future.failedFuture("Store community norm error"));

	}

	/**
	 * Check fail retrieve community norm because repository failed.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldFailRetrieveCommunityNormsPageBecauseRepositoryFails(VertxTestContext testContext) {

		final CommunitiesResource resource = createCommunitiesResource();
		final OperationRequest context = mock(OperationRequest.class);
		doReturn(new JsonObject()).when(context).getParams();
		resource.retrieveCommunityNormsPage("communityId", context, testContext.succeeding(update -> {

			assertThat(update.getStatusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
			testContext.completeNow();
		}));

		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<JsonObject>>> searchHandler = ArgumentCaptor.forClass(Handler.class);
		verify(resource.repository, times(1)).searchCommunityNormsPageObject(eq("communityId"), any(), any(), eq(0), eq(10),
				searchHandler.capture());
		searchHandler.getValue().handle(Future.failedFuture("Search community norm error"));

	}
}
